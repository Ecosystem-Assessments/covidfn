#' Script to get Canadian Community Health Survey, 2017-2018: Annual Component (CCHS)
#'
#' @export

cchs <- function() {
  # Outout folder
  input <- here::here("data", "pipedat_light", "cchs")
  out <- here::here(input, "raw")
  pipedat::chk_create(out)

  # ------------------------------------------------------------------------------------
  # CCHS data
  # Has to be done manually for CCHS at this point...
  # https://borealisdata.ca/dataset.xhtml?persistentId=doi:10.5683/SP3/EYLZ18
  # Citation requirements:
  # Statistics Canada hereby grants to the Licensee a non-exclusive, non-assignable and non-transferable licence to use the Microdata files and related documentation for statistical and research purposes. The Microdata files shall not be used for any other purposes without the prior written consent of Statistics Canada. (Appendix 1, Section 6, DLI Licence Agreement - Microdata Files).
  raw_cchs <- here::here(out, "cchs")
  archive::archive_extract(
    archive = here::here(out, "dataverse_files.zip"),
    dir = raw_cchs
  )

  # Import data
  cchs <- vroom::vroom(here::here(raw_cchs, "CSV", "cchs-82M0013-E-2017-2018-Annual-component_F1.csv"))

  # --------------------------------------------------------------------------------
  # WARNING:
  # The Health Region spatial data (see below) has two HR in Ontario that are combined
  # The ids need to be modified so that, at the next steps:
  #    - the proper data is linked to geographies
  #    - summaries are properly measured
  # ----------------------------------
  # 35939 & 35954 == 35950
  cchs$GEODGHR4[cchs$GEODGHR4 %in% c(35939, 35954)] <- 35950

  # 35931 & 35952 == 35975
  # These are presented as separate in the CCHS. Both HR should thus be summed and their name become 35975
  cchs$GEODGHR4[cchs$GEODGHR4 %in% c(35931, 35952)] <- 35975
  # --------------------------------------------------------------------------------

  # Filtering function
  f_cchs <- function(voi, geo) {
    cls <- stringr::str_split(voi[2], ",") |>
      unlist() |>
      as.numeric()
    vars <- c(geo, voi[1])
    tmp <- cchs[, vars] |>
      data.frame()
    colnames(tmp) <- c("V1", "V2")

    if (voi[3] == "percent") {
      tmp <- tmp |>
        dplyr::group_by(V1, V2) |>
        dplyr::summarise(percent = dplyr::n()) |>
        dplyr::mutate(percent = (percent / sum(percent)) * 100) |>
        dplyr::filter(V2 %in% cls) |>
        dplyr::summarise(percent = sum(percent)) |>
        dplyr::select(V1, percent)
      colnames(tmp) <- vars
    }
    if (voi[3] == "mean") {
      tmp <- tmp |>
        dplyr::filter(!V2 %in% cls) |>
        dplyr::group_by(V1) |>
        dplyr::summarize(mean = mean(V2, na.rm = TRUE))
      colnames(tmp) <- vars
    }
    tmp
  }

  # Variables of interest
  voi <- list(
    # 0053-0053 Perceived health - (D)
    # Class 0: Poor
    # Class 1: Fair
    c("GENDVHDI", "0,1", "percent"),

    # 0054-0054 Perceived mental health - (D)
    # Class 0: Poor
    # Class 1: Fair
    c("GENDVMHI", "0,1", "percent"),

    # 0084-0084 BMI classification 18 and + (adjusted) - intl standard - (D)
    # Class 3: overweight
    # Class 4: Obese - Class I, II, III
    c("HWTDGBCC", "3,4", "percent"),

    # 0088-0088 Has asthma
    # Class 1: Yes
    c("CCC_015", "1", "percent"),

    # 0091-0091 Has a COPD
    # Class 1: Yes
    c("CCC_030", "1", "percent"),

    # 0098-0098 Has high blood pressure
    # Class 1: Yes
    c("CCC_065", "1", "percent"),

    # 0100-0100 Has high blood cholesterol / lipids
    # Class 1: Yes
    c("CCC_075", "1", "percent"),

    # 0102-0102 Has heart disease
    # Class 1: Yes
    c("CCC_085", "1", "percent"),

    # 0104-0104 Has diabetes
    # Class 1: Yes
    c("CCC_095", "1", "percent"),

    # 0113-0113 Has cancer
    # Class 1: Yes
    c("CCC_130", "1", "percent"),

    # 0129-0129 Has an anxiety disorder (phobia, OCD, panic)
    # Class 1: Yes
    c("CCC_200", "1", "percent"),

    # 0251-0252 Number of hours per night usually spent sleeping
    c("SLPG005", "96,99", "mean"),

    # 0319-0319 Type of smoker (daily / occasionally / not at all) - presently
    # Classes 1 (Daily)
    c("SMK_005", "1", "percent"),

    # 0322-0322 Smoked more than 100 cigarettes - lifetime
    # Class 1: Yes
    c("SMK_020", "1", "percent"),

    # 0366-0366 Alternative tobacco product usage - (D)
    # Class 1 (Has used an alternative tobacco product)
    c("TALDVUSE", "1", "percent"),

    # 0386-0386 Type of drinker - 12 months - (D)
    # Class 1: regular drinker
    c("ALCDVTTM", "1", "percent"),

    # 0416-0416 Increased long term risk due to drinking - (D)
    # Class 1: Increased long term health risk due to drinking
    c("ALWDVLTR", "1", "percent"),

    # 0417-0417 Increased short term risks due to drinking - (D)
    # Class 1: Increased short term health risks due to drinking
    c("ALWDVSTR", "1", "percent"),

    # 0447-0449 Active transportation - 7 d - total - hours
    c("PAA_015", "996,997,998,999", "mean"),

    # 0463-0465 Sports / fitness / recreational physical act - 7 d - total - hours
    c("PAA_045", "996,997,998,999", "mean"),

    # 0482-0485 Other physical activities - 7 d - total - minutes
    c("PAA_080", "9996,9997,9999", "mean"),

    # 0490-0493 Physical activities - vigorous intensity - 7 d - total - minutes
    c("PAA_105", "9996,9997,9998,9999", "mean"),

    # 0494-0498 Total minutes - active transportation - 7 d - (D)
    c("PAADVTRV", "99996,99999", "mean"),

    # 0505-0509 Total minutes - recreational physical activities - 7 d - (D)
    c("PAADVREC", "99996,99999", "mean"),

    # 0516-0520 Total minutes - other physical activities - 7 d - (D)
    c("PAADVOTH", "99996,99999", "mean"),

    # 0527-0531 Total minutes - moderate to vigorous physical activities - 7 d - (D)
    c("PAADVMVA", "99996,99999", "mean"),

    # 0534-0538 Total minutes - vigorous physical activities - 7 d - (D)
    c("PAADVVIG", "99996,99999", "mean"),

    # 0539-0543 Volume of weekly activity - 7 d (METs*mins/week) - (D)
    c("PAADVVOL", "99996,99999", "mean"),

    # 0532-0532 Physical activity indicator - (D)
    # Class 2: Physically active below recommended level from CPAG
    c("PAADVACV", "2", "percent"),

    # 0552-0553 Num of days - physically active - 7 d - (D)
    c("PAADVDYS", "96,99", "mean"),

    # 0583-0587 Total minutes - active transportation - 7 d - (D)
    c("PAYDVTTR", "99996,99999", "mean"),

    # 0633-0637 Total minutes - physical activities - 7 d - (D)
    c("PAYDVMNS", "99996,99999", "mean"),

    # 0652-0653 Num of days - meeting / exceeding physical activity guidelines - (D)
    c("PAYDVDPG", "96,99", "mean"),

    # 0655-0659 Total minutes - vigorous physical activity - 7 d - (D)
    c("PAYDVVIG", "99996,99999", "mean"),

    # 0800-0800 Had a seasonal flu shot (excluding H1N1) - lifetime
    # Class 1: Yes
    c("FLU_005", "1", "percent"),

    # 1312-1314 Total usual hours worked per week - (D)
    c("LBFDGHPW", "996,999", "mean"),

    # 1359-1359 Food security - adult status - (D)
    # Class 1: Moderately food insecure
    # Class 2: Severely food insecure
    c("FSCDVAFS", "1,2", "percent"),

    # 1360-1360 Food security - child status - (D)
    # Class 1: Moderately food insecure
    # Class 2: Severely food insecure
    c("FSCDVCFS", "1,2", "percent"),

    # 1361-1361 Household food security status - (D)
    # Class 1: Moderately food insecure
    # Class 2: Severely food insecure
    c("FSCDVHFS", "1,2", "percent")
  )

  # Filter and apply functions to summarize CCHS data
  hr <- lapply(voi, function(x) f_cchs(x, "GEODGHR4")) |>
    purrr::reduce(dplyr::full_join, by = "GEODGHR4") |>
    dplyr::rename(hruid = GEODGHR4)

  # If BC Health Authorities are used
  # bcha <- lapply(voi, function(x) f_cchs(x, "GEODVBHA")) |>
  #   purrr::reduce(dplyr::full_join, by = "GEODVBHA") |>
  #   dplyr::filter(GEODVBHA != 9996) |>
  #   dplyr::rename(hruid = GEODVBHA)
  # hr <- dplyr::bind_rows(hr, bcha)

  # Some HR need to be added to the dataset as they are combined in the CCHS
  # These are identified in the Data Dictionnary document for the GEODGHR4 variable
  # Group: GEODVHR4 = (1013, 1014)
  # Group: GEODVHR4 = (1304, 1305)
  # Group: GEODVHR4 = (1306, 1307)
  # Group: GEODVHR4 = (3539, 3554) # Already modified to fit geographies (see above)
  # Group: GEODVHR4 = (3547, 3563)
  # Group: GEODVHR4 = (4701, 4702, 4703)
  # Group: GEODVHR4 = (4705, 4708)
  # Group: GEODVHR4 = (4707, 4710)
  # Group: GEODVHR4 = (4709, 4714)
  hr <- data.frame(
    hruid = c(
      10914,
      13905,
      13907,
      # 35954,
      35963,
      47902,
      47903,
      47908,
      47910,
      47914
    ),
    from = c(
      10913,
      13904,
      13906,
      # 35939,
      35947,
      47901,
      47901,
      47905,
      47907,
      47909
    )
  ) |>
    dplyr::left_join(hr, by = c("from" = "hruid")) |>
    dplyr::select(-from) |>
    dplyr::bind_rows(hr) |>
    dplyr::arrange(hruid)

  # Remove uncompressed data from disk
  unlink(raw_cchs, recursive = TRUE)

  # ------------------------------------------------------------------------------------
  # Health regions
  raw_hr <- here::here(out, "hr")
  pipedat::chk_create(raw_hr)
  urls <- "https://www150.statcan.gc.ca/n1/en/pub/82-402-x/2023001/hrbf-flrs/carto/GeoData/Cart2022_fgdb-eng.zip?st=oII-mPhJ"
  pipedat::pipeload(
    urls = urls,
    output = raw_hr,
    large = FALSE
  )

  # Unzip HR of interest
  zipfile <- here::here(raw_hr, "HR_000b22f_e.zip")
  archive::archive_extract(
    archive = zipfile,
    dir = raw_hr
  )

  # Import & format
  geo <- sf::st_read("data/pipedat_light/cchs/raw/hr/HR_000b22f_e.gdb", layer = "HR_000b22f_e") |>
    sf::st_simplify(1000, preserveTopology = TRUE) |>
    dplyr::mutate(
      hruid = as.numeric(glue::glue("{substr(HR_UID,1,2)}9{substr(HR_UID,3,4)}"))
    ) |>
    dplyr::select(hruid)

  # Remove files
  files <- dir(raw_hr, full.names = TRUE)
  files <- files[files != zipfile]
  unlink(files)

  # ------------------------------------------------------------------------------------
  # Join with CCHS data
  dat <- dplyr::left_join(geo, hr, by = "hruid") |>
    dplyr::select(-hruid)

  # ------------------------------------------------------------------------------------
  # Prepare names and template to integrate in grid
  datNames <- colnames(sf::st_drop_geometry(dat))
  bb <- sf::st_bbox(dat)
  rt <- raster::raster(
    xmn = bb$xmin, ymn = bb$ymin,
    xmx = bb$xmax, ymx = bb$ymax,
    crs = sf::st_crs(dat)$epsg,
    res = 1000
  ) |>
    stars::st_as_stars()

  # Rasterize
  r <- list()
  for (i in seq_len(length(datNames))) {
    r[[i]] <- stars::st_rasterize(dat[, datNames[i]], rt) |>
      pipedat::masteringrid()
  }

  # ------------------------------------------------------------------------------------
  # Export
  # Vector data
  out <- here::here(input)
  sf::st_write(dat, here::here(out, "cchs.gpkg"), quiet = TRUE, append = FALSE)

  # Gridded data
  out <- here::here(input, "ingrid")
  chk_create(out)
  for (i in seq_len(length(r))) pipedat::masterwrite(r[[i]], here::here(out, datNames[i]))
}
