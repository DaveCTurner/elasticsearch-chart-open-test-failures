{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

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

data RiskLevel
  = LowRisk
  | MediumRisk
  | Blocker
  | NeedsRisk
  deriving (Show, Eq, Ord, Enum)

issueRiskLevel :: Issue -> RiskLevel
issueRiskLevel = go Nothing . _issueLabels
  where
    go acc [] = case acc of Nothing -> Blocker; Just rl -> rl
    go acc (l : ls) = go (step acc (_labelName l)) ls

    step acc = \case
      "low-risk"    -> max acc (Just LowRisk)
      "medium-risk" -> max acc (Just MediumRisk)
      "blocker"     -> max acc (Just Blocker)
      "needs:risk"  -> max acc (Just NeedsRisk)
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

data PangoLabel = PangoLabel
  { _pangoLabelLayout           :: PangoLayout
  , _pangoLabelInkRectangle     :: PangoRectangle
  , _pangoLabelLogicalRectangle :: PangoRectangle
  }

pangoRectangleWidth :: PangoRectangle -> Double
pangoRectangleWidth (PangoRectangle x0 _ x1 _) = x1 - x0

pangoRectangleHeight :: PangoRectangle -> Double
pangoRectangleHeight (PangoRectangle _ y0 _ y1) = y1 - y0

roundPixel :: Double -> Double
roundPixel v = fromIntegral (ceiling v :: Int)

pangoLabelHeight :: PangoLabel -> Double
pangoLabelHeight PangoLabel{..} = roundPixel $ pangoRectangleHeight _pangoLabelLogicalRectangle

pangoLabelWidth:: PangoLabel -> Double
pangoLabelWidth PangoLabel{..} = roundPixel $ pangoRectangleWidth _pangoLabelLogicalRectangle

pangoLabelInkXOffset :: PangoLabel -> Double
pangoLabelInkXOffset PangoLabel {_pangoLabelLogicalRectangle = PangoRectangle lx0 _ _ _, _pangoLabelInkRectangle = PangoRectangle ix0 _ _ _} = ix0 - lx0

pangoLabelInkYOffset :: PangoLabel -> Double
pangoLabelInkYOffset PangoLabel {_pangoLabelLogicalRectangle = PangoRectangle _ ly0 _ _, _pangoLabelInkRectangle = PangoRectangle _ iy0 _ _} = iy0 - ly0

renderPangoLabelTopCentre :: PangoLabel -> Double -> Double -> Render ()
renderPangoLabelTopCentre l@PangoLabel{..} xCentre yTop = do
  moveTo (roundPixel (xCentre - (pangoRectangleWidth _pangoLabelInkRectangle / 2) - pangoLabelInkXOffset l)) (roundPixel yTop)
  showLayout _pangoLabelLayout

renderPangoLabelTopLeft :: PangoLabel -> Double -> Double -> Render ()
renderPangoLabelTopLeft PangoLabel{..} xLeft yTop = do
  moveTo (roundPixel xLeft) (roundPixel yTop)
  showLayout _pangoLabelLayout

renderPangoLabelMiddleRight :: PangoLabel -> Double -> Double -> Render ()
renderPangoLabelMiddleRight l@PangoLabel{..} xRight yMiddle = do
  moveTo (roundPixel $ xRight - pangoRectangleWidth _pangoLabelInkRectangle - pangoLabelInkXOffset l)
         (roundPixel $ yMiddle - (pangoRectangleHeight _pangoLabelInkRectangle / 2) - pangoLabelInkYOffset l)
  showLayout _pangoLabelLayout

data TeamData = TeamData
  { _teamDataName       :: T.Text
  , _teamDataIssueAges  :: M.Map RiskLevel [Double]
  , _teamDataIssueCount :: Int
  , _teamDataNameLabel  :: PangoLabel
  }

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
            searchResponse <- getWith opts url
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
        case mSr of
          Nothing -> error $ "failed to decode page " ++ show pageNum
          Just (SearchResponseBody issues) -> do
            forM_ issues $ \issue -> when (issueTeamNames issue == ["Missing"]) $ print issue
            return $ M.unionsWith (M.unionWith (++)) (
              acc : [ M.singleton teamName (M.singleton riskLevel [issueAgeDays now issue])
                    | issue     <- issues
                    , riskLevel <- [LowRisk .. issueRiskLevel issue]
                    , teamName <- issueTeamNames issue
                    ])
    )
    M.empty
    [0..lastResultsPage]

  pangoContext <- cairoCreateContext . Just =<< cairoFontMapGetDefault
  titleFontDescription <- fontDescriptionFromString ("Sans Bold" :: String)
  fontDescriptionSetSize titleFontDescription 16.0
  teamNameFontDescription <- fontDescriptionFromString ("Sans Bold" :: String)
  fontDescriptionSetSize teamNameFontDescription 12.0
  labelFontDescription <- fontDescriptionFromString ("Sans" :: String)
  fontDescriptionSetSize labelFontDescription 8.0

  let pangoLabel :: FontDescription -> String -> IO PangoLabel
      pangoLabel font text = do
        _pangoLabelLayout <- layoutText pangoContext text
        layoutSetFontDescription _pangoLabelLayout $ Just font
        (_pangoLabelInkRectangle, _pangoLabelLogicalRectangle) <- layoutGetExtents _pangoLabelLayout
        return $ PangoLabel{..}

      pangoAxisLabel  = pangoLabel labelFontDescription

  titleLabel <- pangoLabel titleFontDescription $ "Cumulative open test failures by age, per team and risk level - " ++ show (utctDay now)
  ageAxisLabel <- pangoAxisLabel "Age (weeks)"

  let teamCount :: Int
      teamCount  = M.size issueTimesByTeamAndRisk

      issueCount :: Int
      issueCount = sum
        [ M.foldr (\i n -> max n $ length i) 11 m
        | m <- M.elems issueTimesByTeamAndRisk
        ]

      issueHeight :: Double
      issueHeight = 5.0

      dayWidth :: Double
      dayWidth = 3.0

      dayCount :: Double
      dayCount = 365.0

      margin :: Double
      margin = 30.0

  weekAgeLabels <- forM [0,7..floor dayCount] $ \vGrid -> (vGrid,) <$> pangoAxisLabel (show $ vGrid `div` (7::Int))

  teamData <- forM (M.toList issueTimesByTeamAndRisk) $ \(_teamDataName, _teamDataIssueAges) -> do
    let _teamDataIssueCount = M.foldr (\i n -> max n $ length i) 11 _teamDataIssueAges
    _teamDataNameLabel <- pangoLabel teamNameFontDescription $ T.unpack _teamDataName
    return TeamData{..}

  issueCountLabels <- forM [0,10..maximum $ map _teamDataIssueCount teamData] $ \hGrid -> pangoAxisLabel (show hGrid)

  let titleHeight :: Double
      titleHeight = pangoLabelHeight titleLabel

      teamNameHeight :: Double
      teamNameHeight = maximum $ map (pangoLabelHeight . _teamDataNameLabel) teamData

      axisMarkHeight :: Double
      axisMarkHeight = maximum $ map (pangoLabelHeight . snd) weekAgeLabels

      axisLabelHeight :: Double
      axisLabelHeight = pangoLabelHeight ageAxisLabel

      axisLabelVSpace :: Double
      axisLabelVSpace = 5.0

      axisMarkWidth = maximum $ map pangoLabelWidth issueCountLabels

      imageWidth, imageHeight :: Int
      imageWidth  = ceiling $ margin * 2.0 + dayCount * dayWidth + axisMarkWidth
      imageHeight = ceiling $ margin * 2.0 + fromIntegral teamCount * (teamNameHeight + axisMarkHeight + axisLabelHeight + axisLabelVSpace*2) + fromIntegral issueCount * issueHeight + titleHeight

      setColor :: Int -> Int -> Int -> Render ()
      setColor rr gg bb = setSourceRGB (fromIntegral rr / 255.0) (fromIntegral gg / 255.0) (fromIntegral bb / 255.0)

      renderTeam :: TeamData -> Render ()
      renderTeam TeamData{..} = do
        do
          setSourceRGB 0.0 0.0 0.0
          renderPangoLabelTopLeft _teamDataNameLabel 5 0
        translate 0 teamNameHeight

        let issueCounts = _teamDataIssueAges
        let teamIssueCount = _teamDataIssueCount

        translate 0 (fromIntegral teamIssueCount * issueHeight)

        forM_ (M.toList issueCounts) $ \(riskLevel, issueAges) -> do
          case riskLevel of
            LowRisk    -> setColor 0xef 0xfd 0x5f
            MediumRisk -> setColor 0xed 0x70 0x14
            Blocker    -> setColor 0xff 0x80 0x80
            NeedsRisk  -> setColor 0xc5 0xde 0xf5
          moveTo 0 0
          forM_ (zip [(1::Int)..] issueAges) $ \(count, age) -> do
            lineTo (roundPixel (min age dayCount * dayWidth)) (roundPixel (negate $ fromIntegral (count - 1) * issueHeight))
            lineTo (roundPixel (min age dayCount * dayWidth)) (roundPixel (negate $ fromIntegral  count      * issueHeight))
          lineTo 0 (roundPixel (negate $ fromIntegral (length issueAges) * issueHeight))
          fill

        forM_ (zip [0,10..teamIssueCount] issueCountLabels) $ \(hGrid, issueCountLabel) -> do
          let yy = (+0.5) $ roundPixel $ negate $ fromIntegral hGrid * issueHeight
          setLineWidth 1.0
          setSourceRGBA 0.0 0.0 0.0 0.2
          moveTo 0                     yy
          lineTo (dayCount * dayWidth) yy
          stroke

          setSourceRGB 0.0 0.0 0.0
          renderPangoLabelMiddleRight issueCountLabel (-5) (yy-1)

        forM_ weekAgeLabels $ \(vGrid, weekAgeLabel) -> do
          let xx = (+0.5) $ roundPixel $ fromIntegral (vGrid::Int) * dayWidth
          setLineWidth 1.0
          setSourceRGBA 0.0 0.0 0.0 0.2
          moveTo xx 0
          lineTo xx (negate (fromIntegral teamIssueCount) * issueHeight)
          stroke

          setSourceRGB 0.0 0.0 0.0
          renderPangoLabelTopCentre weekAgeLabel xx axisLabelVSpace

        setSourceRGB 0.0 0.0 0.0
        setLineWidth 2.0
        moveTo 0 (negate (fromIntegral teamIssueCount) * issueHeight)
        lineTo 0 0
        lineTo (dayCount * dayWidth) 0
        stroke

        translate 0 (axisMarkHeight + axisLabelVSpace)
        renderPangoLabelTopCentre ageAxisLabel (dayCount * dayWidth / 2) axisLabelVSpace
        translate 0 (axisLabelHeight + axisLabelVSpace)

  withImageSurface FormatARGB32 imageWidth imageHeight $ \surface -> do
    renderWith surface $ do
      setSourceRGB 1.0 1.0 1.0
      rectangle 0 0 (fromIntegral imageWidth) (fromIntegral imageHeight)
      fill
      translate (margin + axisMarkWidth) margin
      do
        setSourceRGB 0.0 0.0 0.0
        renderPangoLabelTopCentre titleLabel (dayCount * dayWidth / 2) 0
        translate 0 titleHeight
      mapM_ renderTeam teamData
    surfaceWriteToPNG surface "test-failures.png"

  do
    helloLayout <- layoutText pangoContext ("Hej!"::String)
    fontDescription <- fontDescriptionFromString ("Sans Bold" :: String)
    fontDescriptionSetSize fontDescription 120.0
    layoutSetFontDescription helloLayout $ Just fontDescription
    (PangoRectangle ix0 iy0 ix1 iy1, PangoRectangle lx0 ly0 lx1 ly1) <- layoutGetExtents helloLayout
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
