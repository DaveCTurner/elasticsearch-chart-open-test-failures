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
import Data.Char (isDigit)
import Data.List (sort, stripPrefix)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Time
import Graphics.Rendering.Cairo
import Graphics.Rendering.Pango
import Network.URI (escapeURIString, isUnreserved)
import Network.Wreq
import Options.Applicative hiding (header)
import System.Directory
import System.IO
import System.Process.Typed (proc, readProcessStdout_)
import Text.Read (readMaybe)

import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Options.Applicative as OA

data RunConfig = RunConfig
  { _runConfigRefreshData :: Bool
  , _runConfigTeamNames   :: [T.Text]
  } deriving (Show, Eq)

runConfigParser :: Parser RunConfig
runConfigParser = RunConfig
  <$> refreshDataParser
  <*> teamNamesParser
  where
    refreshDataParser :: Parser Bool
    refreshDataParser = flag False True
        (  long "refresh-data"
        <> help "Retrieve data from Github")

    teamNamesParser :: Parser [T.Text]
    teamNamesParser = map T.pack <$> many (strOption
        (  long "team"
        <> metavar "TEAM_NAME"
        <> help "Team name to include (repeatable)"))

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
  , _issueUpdated    :: UTCTime
  , _issueLabels     :: [Label]
  } deriving (Show, Eq)

instance FromJSON Issue where
  parseJSON = withObject "Issue" $ \v -> Issue
    <$> v .: "html_url"
    <*> v .: "title"
    <*> v .: "number"
    <*> v .: "repository_url"
    <*> v .: "created_at"
    <*> v .: "updated_at"
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
    go acc [] = fromMaybe Blocker acc
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

-- | Age of the issue in days at @analysisTime@ (floating-point).
issueAgeDays :: UTCTime -> Issue -> Double
issueAgeDays analysisTime issue =
  realToFrac (diffUTCTime analysisTime (_issueCreated issue)) / realToFrac nominalDay

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

resultsPageFileName :: Int -> String
resultsPageFileName pageNum = "results-page-" ++ show pageNum ++ ".json"

main :: IO ()
main = do
  RunConfig{..} <- execParser runConfigParserInfo

  let isSelectedTeamName = case _runConfigTeamNames of
        [] -> \_ -> True
        _  -> flip S.member $ S.fromList _runConfigTeamNames

  oldLastResultsPage <- (maximum . ((-1):) . mapMaybe parseResultsPageNum) <$> listDirectory "."

  lastResultsPage <- if _runConfigRefreshData
    then do
      token <- BL.toStrict
            <$> B64.decodeLenient
            <$> fromMaybe (error "GitHub keychain entry did not start with go-keyring-base64:")
            <$> BL.stripPrefix "go-keyring-base64:"
            <$> readProcessStdout_ (proc "security" ["find-generic-password", "-s", "gh:github.com", "-w"])
      let opts = defaults
            & header "Accept"        .~ ["application/vnd.github.v3+json"]
            & header "Authorization" .~ ["token " <> token]
          getResultsPages pageNum url = do
            when (pageNum > (10::Int)) $ threadDelay 1000000
            when (pageNum > (15::Int)) $ threadDelay 4000000
            searchResponse <- getWith opts url
            withFile ("results-page-" ++ show pageNum ++ ".json") WriteMode $ \h -> BL.hPutStr h $ searchResponse ^. responseBody
            case searchResponse ^? responseLink "rel" "next" . linkURL of
              Just url' -> getResultsPages (pageNum + 1) $ T.unpack $ TE.decodeUtf8 url'
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

      forM_ (map resultsPageFileName [nextFreePage..oldLastResultsPage]) $ \fileName -> do
        fileExists <- doesFileExist fileName
        when fileExists $ removeFile fileName
      return $ nextFreePage - 1
    else
      return oldLastResultsPage

  allIssues <- fmap concat $ forM (map resultsPageFileName [0..lastResultsPage])
    $ \fileName -> maybe (error $ "failed to decode page " ++ fileName) _srIssues <$> decodeFileStrict fileName

  forM_ allIssues $ \issue -> when (issueTeamNames issue == ["Missing"]) $ print issue

  analysisTime <- case allIssues of
    [] -> getCurrentTime
    _  -> return $ maximum $ concatMap (\issue -> [_issueCreated issue, _issueUpdated issue]) allIssues

  let issueTimesByTeamAndRisk = M.map (M.map (reverse . sort)) $
        M.unionsWith (M.unionWith (++))
          [ M.singleton teamName (M.singleton riskLevel [issueAgeDays analysisTime issue])
          | issue     <- allIssues
          , riskLevel <- [LowRisk .. issueRiskLevel issue]
          , teamName  <- issueTeamNames issue
          , isSelectedTeamName teamName
          ]

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

  titleLabel <- pangoLabel titleFontDescription $ "Cumulative open test failures by age, per team and risk level - " ++ show (utctDay analysisTime)
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
