{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Lens
import Control.Monad
import Data.Aeson
import Data.String.Utils (strip)
import Data.Time
import Data.Char (isDigit)
import Data.List (sort, stripPrefix)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import qualified Data.Map.Strict as M
import Network.URI (escapeURIString, isUnreserved)
import Network.Wreq
import Options.Applicative hiding (header)
import System.Directory
import System.IO
import Graphics.Rendering.Cairo
import Graphics.Rendering.Pango
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified System.Process as SP
import qualified Options.Applicative as OA

data RunConfig = RunConfig
  { _runConfigRefreshData :: Bool
  } deriving (Show, Eq)

runConfigParser :: Parser RunConfig
runConfigParser = RunConfig
  <$> refreshDataParser
  where
    refreshDataParser :: Parser Bool
    refreshDataParser = flag False True
        (  long "refresh-data"
        <> help "Retrieve data from Github")

runConfigParserInfo :: ParserInfo RunConfig
runConfigParserInfo = info (runConfigParser <**> helper)
    (fullDesc
        <> progDesc "Visualize open test failures"
        <> OA.header "open-test-failures - Generate visualisation of open test failures")




data SearchResponseBody = SearchResponseBody
  { _srIssues :: [Issue]
  } deriving (Show, Eq)

instance FromJSON SearchResponseBody where
  parseJSON = withObject "SearchResponseBody" $ \v -> SearchResponseBody
    <$> v .: "items"

data Label = Label
  { _labelName :: T.Text
  } deriving (Show, Eq, Ord)

instance FromJSON Label where
  parseJSON = withObject "Label" $ \v -> Label
    <$> v .: "name"

data Issue = Issue
  { _issueHtmlUrl    :: T.Text
  , _issueTitle      :: T.Text
  , _issueNumber     :: Integer
  , _issueRepository :: T.Text
  , _issueCreated    :: UTCTime
  , _issueLabels     :: [Label]
  } deriving (Show, Eq)

instance FromJSON Issue where
  parseJSON = withObject "Issue" $ \v -> Issue
    <$> v .: "html_url"
    <*> v .: "title"
    <*> v .: "number"
    <*> v .: "repository_url"
    <*> v .: "created_at"
    <*> v .: "labels"

issueRiskLevel :: Issue -> Int
issueRiskLevel = go 0 . _issueLabels
  where
    go acc [] = if acc == 0 then 3 else acc
    go acc (l : ls) = go (step acc (_labelName l)) ls

    step acc = \case
      "low-risk"    -> max acc 1
      "medium-risk" -> max acc 2
      "blocker"     -> max acc 3
      "needs:risk"  -> max acc 4
      _             -> acc

issueTeamNames :: Issue -> [T.Text]
issueTeamNames issue = if null kept then ["Missing"] else kept
  where
    kept = [ T.drop 5 labelName
           | Label labelName <- _issueLabels issue
           , "Team:" `T.isPrefixOf` labelName
           ]

-- | Age of the issue in days at @now@ (floating-point).
issueAgeDays :: UTCTime -> Issue -> Double
issueAgeDays now issue =
  realToFrac (diffUTCTime now (_issueCreated issue)) / realToFrac nominalDay

-- | Parse @results-page-0.json@ style names; only a digit sequence before @.json@ matches.
parseResultsPageNum :: FilePath -> Maybe Int
parseResultsPageNum name = do
  rest <- stripPrefix "results-page-" name
  let (digits, suffix) = span isDigit rest
  guard $ suffix == ".json" && not (null digits)
  readMaybe digits

main :: IO ()
main = do
  now <- getCurrentTime
  RunConfig{..} <- execParser runConfigParserInfo

  oldLastResultsPage <- (maximum . ((-1):) . mapMaybe parseResultsPageNum) <$> listDirectory "."

  lastResultsPage <- if _runConfigRefreshData
    then do
      token <- strip <$> SP.readProcess "security" ["find-generic-password", "-s", "github-recent-issues", "-w"] ""
      let opts = defaults
            & header "Accept"        .~ ["application/vnd.github.v3+json"]
            & header "Authorization" .~ [B.append "token " $ T.encodeUtf8 $ T.pack token]
          getResultsPages pageNum url = do
            when (pageNum > (10::Int)) $ threadDelay 1000000
            when (pageNum > (15::Int)) $ threadDelay 4000000
            searchResponse <- getWith _opts url
            withFile ("results-page-" ++ show pageNum ++ ".json") WriteMode $ \h -> BL.hPutStr h $ searchResponse ^. responseBody
            case searchResponse ^? responseLink "rel" "next" . linkURL of
              Just url' -> getResultsPages (pageNum + 1) $ T.unpack $ T.decodeUtf8 url'
              Nothing   -> return (pageNum + 1)

          getRepositoryResultsPages pageNum repository =
            getResultsPages pageNum $
              "https://api.github.com/search/issues?per_page=100&q="
                ++ escapeURIString isUnreserved ("repo:" ++ repository ++ " is:issue state:open label:>test-failure")
                ++ "&sort=updated&advanced_search=true"

      nextFreePage <- foldM getRepositoryResultsPages 0
        [ "elastic/elasticsearch"
        , "elastic/elasticsearch-serverless"
        ]

      forM_ [nextFreePage..oldLastResultsPage] $ \pageNum -> do
        let fileName = "results-page-" ++ show pageNum ++ ".json"
        fileExists <- doesFileExist fileName
        when fileExists $ removeFile fileName
      return $ nextFreePage - 1
    else
      return oldLastResultsPage

  issueTimesByTeamAndRisk <- M.map (M.map (reverse . sort)) <$> foldM
    ( \acc pageNum -> do
        mSr <- decodeFileStrict $ "results-page-" ++ show pageNum ++ ".json"
        pure $ case mSr of
          Nothing -> error $ "failed to decode page " ++ show pageNum
          Just (SearchResponseBody issues) ->
            M.unionsWith (M.unionWith (++)) (
              acc : [ M.singleton teamName (M.singleton riskLevel [issueAgeDays now issue])
                    | issue     <- issues
                    , riskLevel <- [1 .. issueRiskLevel issue]
                    , teamName <- issueTeamNames issue
                    ])
    )
    M.empty
    [0..lastResultsPage]

  pangoContext <- cairoCreateContext . Just =<< cairoFontMapGetDefault
  titleFontDescription <- fontDescriptionFromString ("Sans Bold" :: String)
  fontDescriptionSetSize titleFontDescription 12.0
  labelFontDescription <- fontDescriptionFromString ("Sans" :: String)
  fontDescriptionSetSize labelFontDescription 8.0

  let teamCount :: Int
      teamCount  = M.size issueTimesByTeamAndRisk

      issueCount :: Int
      issueCount = sum
        [ M.foldr (\i n -> max n $ length i) 11 m
        | m <- M.elems issueTimesByTeamAndRisk
        ]

      titleHeight :: Double
      titleHeight = 30.0

      teamNameHeight :: Double
      teamNameHeight = 25.0

      axisLabelHeight :: Double
      axisLabelHeight = 25.0

      issueHeight :: Double
      issueHeight = 5.0

      dayWidth :: Double
      dayWidth = 3.0

      dayCount :: Double
      dayCount = 365.0

      margin :: Double
      margin = 30.0

      imageWidth, imageHeight :: Int
      imageWidth  = ceiling $ margin * 2.0 + dayCount * dayWidth
      imageHeight = ceiling $ margin * 2.0 + fromIntegral teamCount * (teamNameHeight + axisLabelHeight) + fromIntegral issueCount * issueHeight + titleHeight

      renderTeams :: [(T.Text, M.Map Int [Double])] -> Render ()
      renderTeams [] = return ()
      renderTeams ((teamName, issueCounts):ts) = renderTeam teamName issueCounts >> renderTeams ts

      setColor :: Int -> Int -> Int -> Render ()
      setColor rr gg bb = setSourceRGB (fromIntegral rr / 255.0) (fromIntegral gg / 255.0) (fromIntegral bb / 255.0)

      roundPixel :: Double -> Double
      roundPixel v = fromIntegral (floor v :: Int)

      renderTeam :: T.Text -> M.Map Int [Double] -> Render ()
      renderTeam teamName issueCounts = do
        do
          setSourceRGB 0.0 0.0 0.0
          pangoLayout <- createLayout (T.unpack teamName)
          liftIO $ layoutSetFontDescription pangoLayout $ Just labelFontDescription
          PangoRectangle _x0 y0 _x1 y1 <- liftIO $ fst <$> layoutGetExtents pangoLayout
          moveTo 5 (roundPixel $ teamNameHeight - (y1 - y0) - 5)
          showLayout pangoLayout
        translate 0 teamNameHeight

        let teamIssueCount = M.foldr (\i n -> max n $ length i) 11 issueCounts
        translate 0 (fromIntegral teamIssueCount * issueHeight)

        forM_ (M.toList issueCounts) $ \(riskLevel, issueAges) -> do
          if | riskLevel == 1 -> setColor 0xef 0xfd 0x5f
             | riskLevel == 2 -> setColor 0xed 0x70 0x14
             | riskLevel == 3 -> setColor 0xff 0x80 0x80
             | riskLevel == 4 -> setColor 0xc5 0xde 0xf5
             | otherwise      -> error "unknown risk level"
          moveTo 0 0
          forM_ (zip [(1::Int)..] issueAges) $ \(count, age) -> do
            lineTo (roundPixel (min age dayCount * dayWidth)) (roundPixel (negate $ fromIntegral (count - 1) * issueHeight))
            lineTo (roundPixel (min age dayCount * dayWidth)) (roundPixel (negate $ fromIntegral  count      * issueHeight))
          lineTo 0 (roundPixel (negate $ fromIntegral (length issueAges) * issueHeight))
          fill

        forM_ [0,10..teamIssueCount] $ \hGrid -> do
          let yy = (+0.5) $ roundPixel $ negate $ fromIntegral hGrid * issueHeight
          setLineWidth 1.0
          setSourceRGBA 0.0 0.0 0.0 0.2
          moveTo 0                     yy
          lineTo (dayCount * dayWidth) yy
          stroke

          setSourceRGB 0.0 0.0 0.0
          pangoLayout <- createLayout (show hGrid)
          liftIO $ layoutSetFontDescription pangoLayout $ Just labelFontDescription
          PangoRectangle _ y0 x1 y1 <- liftIO $ snd <$> layoutGetExtents pangoLayout
          moveTo (roundPixel $ negate $ x1+8) (roundPixel $ yy - (y1-y0)/2 )
          showLayout pangoLayout

        forM_ [0,7..floor dayCount] $ \vGrid -> do
          let xx = (+0.5) $ roundPixel $ fromIntegral (vGrid::Int) * dayWidth
          setLineWidth 1.0
          setSourceRGBA 0.0 0.0 0.0 0.2
          moveTo xx 0
          lineTo xx (negate (fromIntegral teamIssueCount) * issueHeight)
          stroke

          setSourceRGB 0.0 0.0 0.0
          pangoLayout <- createLayout (show (div vGrid 7))
          liftIO $ layoutSetFontDescription pangoLayout $ Just labelFontDescription
          PangoRectangle x0 _ x1 y1 <- liftIO $ fst <$> layoutGetExtents pangoLayout
          moveTo (roundPixel $ xx - (x1-x0)/2) (roundPixel $ y1+1)
          showLayout pangoLayout

        do
          pangoLayout <- createLayout ("age (weeks)"::String)
          liftIO $ layoutSetFontDescription pangoLayout $ Just labelFontDescription
          PangoRectangle x0 _ x1 y1 <- liftIO $ fst <$> layoutGetExtents pangoLayout
          moveTo (roundPixel $ (dayCount * dayWidth - (x1-x0))/2) (roundPixel $ y1+15)
          showLayout pangoLayout

        setSourceRGB 0.0 0.0 0.0
        setLineWidth 2.0
        moveTo 0 (negate (fromIntegral teamIssueCount) * issueHeight)
        lineTo 0 0
        lineTo (dayCount * dayWidth) 0
        stroke

        translate 0 axisLabelHeight

  withImageSurface FormatARGB32 imageWidth imageHeight $ \surface -> do
    renderWith surface $ do
      setSourceRGB 1.0 1.0 1.0
      rectangle 0 0 (fromIntegral imageWidth) (fromIntegral imageHeight)
      fill
      translate margin margin
      do
        setSourceRGB 0.0 0.0 0.0
        pangoLayout <- createLayout ("Cumulative open test failures by age, per team and risk level - " ++ show (utctDay now))
        liftIO $ layoutSetFontDescription pangoLayout $ Just titleFontDescription
        PangoRectangle x0 y0 x1 y1 <- liftIO $ fst <$> layoutGetExtents pangoLayout
        moveTo (roundPixel $ (fromIntegral imageWidth - (x1-x0))/2) (roundPixel $ (titleHeight - (y1-y0))/2)
        showLayout pangoLayout
        translate 0 titleHeight
      renderTeams (M.toList issueTimesByTeamAndRisk)
    surfaceWriteToPNG surface "test-failures.png"

  do
    helloLayout <- layoutText pangoContext ("Hej!"::String)
    fontDescription <- fontDescriptionFromString ("Sans Bold" :: String)
    fontDescriptionSetSize fontDescription 120.0
    layoutSetFontDescription helloLayout $ Just fontDescription
    extents@(PangoRectangle ix0 iy0 ix1 iy1, PangoRectangle lx0 ly0 lx1 ly1) <- layoutGetExtents helloLayout
    print extents
    withImageSurface FormatARGB32 imageWidth imageHeight $ \surface -> do
      renderWith surface $ do
        setSourceRGB 0 0 0
        translate 500 500
        moveTo 0 0
        showLayout helloLayout
        setSourceRGB 1 0 0
        rectangle lx0 ly0 lx1 ly1
        stroke
        setSourceRGB 0 1 0
        rectangle ix0 iy0 ix1 iy1
        stroke
        setSourceRGB 0 0 01
        moveTo (-5) (-5)
        lineTo   5    5
        stroke
        moveTo (-5)   5
        lineTo   5  (-5)
        stroke
      surfaceWriteToPNG surface "test.png"
    return ()

  -- withImageSurface FormatARGB32 1000 1000 $ \surface -
