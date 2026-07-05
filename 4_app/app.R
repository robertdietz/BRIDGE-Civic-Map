library(tidyverse)
library(sf)
library(leaflet)
library(shiny)
library(tools)
library(bslib)
library(tools)

rds_files <- list.files(pattern = "\\.rds$", full.names = TRUE)

obj_names <- file_path_sans_ext(basename(rds_files))

rds_files %>%
  set_names(obj_names) %>%
  map(readRDS) %>%
  list2env(envir = .GlobalEnv)

formatters <- list(
  percent = function(x) paste0(round(x, 1), "%"),
  count   = function(x) format(round(x, 0), big.mark = ","),
  dollar  = function(x) paste0("$", format(round(x, 0), big.mark = ","))
)

category_choices   <- sort(unique(var_config$category))
region_categories  <- c(sort(unique(overlay_region$category)))
transit_levels <- c(
  "Library Branches",
  "Rail: All",
  "Rail: Trolley",
  "Rail: High Speed",
  "Rail: Regional",
  "Schools by Grade: All",
  "Schools by Grade: Elementary",
  "Schools by Grade: Middle",
  "Schools by Grade: High",
  "Schools by Type: All",
  "Schools by Type: Archdiocese",
  "Schools by Type: Charter",
  "Schools by Type: Contracted",
  "Schools by Type: District",
  "Schools by Type: Private"
)

district_levels <- c(
  "District 1",
  "District 2", 
  "District 3",
  "District 4", 
  "District 5",
  "District 6", 
  "District 7",
  "District 8", 
  "District 9",
  "District 10"
)

transit_categories <- factor(transit_levels, levels = transit_levels)
#______________________----
#UI----
ui <- fluidPage(
  theme = bs_theme(version = 3),
  
  div(
    id = "responsive-screen-blocker",
    div(
      class = "blocker-content",
      div(class = "blocker-title", "Screen Width Too Narrow"),
      div(class = "blocker-text", 
          "The BRIDGE Civic Mapping tool is designed for side by side map comparisons.",
          "To view this application, please expand your window, rotate your tablet to landscape mode, or use a larger screen (minimum 1024px wide)."
      )
    )
  ),
  tags$head(
    tags$link(
      rel  = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Roboto:wght@400;500;700&display=swap"
    ),
##*HTML Style Tags----
    tags$style(HTML("
/* === Base layout === */
      html, body {
        height: 100%;
        overflow: hidden;
        min-width: 1023px;
        font-family: 'Roboto', sans-serif;
      }
      .container-fluid {
        height: 100%;
        display: flex;
        flex-direction: column;
        padding: 0 !important;
        min-width: 900px;
        overflow-x: auto;
      }

/* === Selectize dropdown behavior === */
      .selectize-dropdown    { z-index: 9999 !important; box-shadow: none !important; border: 1px solid #cbd5e1 !important; }
      .selectize-input.focus { box-shadow: none !important; }
      
      /* FIX: Pin the fade effect to the container edge, not the text length */
      .selectize-input .item {
        white-space: nowrap !important;
        overflow: hidden !important;        
        text-overflow: clip !important;     
        
        /* 1. Force the item bounding box to push all the way to the right edge */
        width: 100% !important;         
        display: block !important;
        text-align: left !important;
        
        /* 2. Anchor the fade using exact pixels instead of percentages */
        /* This stays solid black until exactly 25px from the edge, then fades */
        -webkit-mask-image: linear-gradient(to right, black calc(100% - 35px), transparent 100%) !important;
        mask-image: linear-gradient(to right, black calc(100% - 35px), transparent 100%) !important;
      }

      /* === Form spacing resets === */
      .form-group { margin-bottom: 0; }
      .row        { margin-left: 0 !important; margin-right: 0 !important; }

      /* === Outlined Floating Label Input Blocks === */
      .input-block .form-group {
        position: relative !important;
        margin: 6px 0 !important;
        padding: 0 !important;
        width: 100%;
      }
      
      /* Base label resting inside the frame */
      .input-block label.control-label {
        position: absolute !important;
        left: 12px !important;
        top: 50% !important;
        transform: translateY(-50%) !important;
        background: transparent !important;
        padding: 0 4px !important;
        color: #64748b !important;
        font-size: 14px !important;
        font-weight: 400 !important;
        transition: all 0.2s ease-in-out !important;
        pointer-events: none !important;
        z-index: 10 !important;
        margin: 0 !important;
        line-height: 1 !important;
      }

/* Base selectize frame design */
      .input-block .selectize-input {
        border: 1px solid #cbd5e1 !important;
        border-radius: 6px !important;
        min-height: 36px !important;
        padding-top: 2px !important;
        padding-bottom: 2px !important;
        padding-left: 15px !important;
        padding-right: 6px !important; 
        background: #ffffff !important;
        box-shadow: none !important;
        display: flex !important;
        align-items: center !important;
        justify-content: flex-start !important; 
        transition: border-color 0.2s ease-in-out, box-shadow 0.2s ease-in-out !important;
      }

      /* FIX: Let text run under arrow and hard-cut at the far right edge */
      .selectize-input .item {
        white-space: nowrap !important;
        overflow: hidden !important;       
        text-overflow: clip !important;     
        width: 100% !important;             
        display: block !important;
        text-align: left !important;
      }

      /* Trigger state: Transform and float label to the top border when focused or filled */
      .input-block .form-group:has(.selectize-input.focus) label.control-label,
      .input-block .form-group.is-filled label.control-label {
        top: 0px !important;
        transform: translateY(-50%) scale(0.85) !important;
        transform-origin: left center !important; 
        background-color: #ffffff !important;
        font-size: 12px !important;
        font-weight: 500 !important;
        color: #334155 !important;
        z-index: 11 !important;
      }

      /* Clean dropdown indicator alignment */
      .input-block .selectize-control.single .selectize-input:after {
        position: absolute !important; /* 3. FIX: Keeps arrow pinned on the right, independent of the flex grid text length */
        top: 50% !important;
        transform: translateY(-50%) !important;
        margin-top: 0 !important;
        right: 14px !important;
        border-color: #000000  transparent transparent transparent !important;
        transition: border-color 0.2s !important;
      }
      .input-block .selectize-control.single .selectize-input.dropdown-active:after {
        border-color: transparent transparent #000000  transparent !important;
        margin-top: -3px !important;
      }

      /* === Map A / B control bars === */
      #mapA-controls,
      #mapB-controls {
        padding-top: 2px !important;
        padding-bottom: 2px !important;
        margin-bottom: 0 !important;
        height: auto !important;
      }

      /* === Modal === */
      .modal-dialog {
        display: flex;
        align-items: center;
        min-height: calc(100% - 60px);
        margin: 30px auto;
      }
      .modal-content { width: 100%; box-shadow: none; }

      /* === Leaflet overrides === */
      .leaflet-popup-content         { font-size: 14px; }
      .leaflet-popup-content-wrapper { opacity: 0.85; box-shadow: none !important; }
      .leaflet-popup-tip-container,
      .leaflet-popup-tip             { box-shadow: none !important; filter: none !important; }
      .info.legend                   { box-shadow: none !important; max-width: 160px; }

      /* === Row 1 cell defaults === */
      .header-cell {
        display: flex !important;
        align-items: center !important;
        padding: 0 8px !important;
        overflow: visible !important; 
      }

      /* 1. Reset Shiny's structural wrappers */
      .header-cell .form-group,
      .header-cell .checkbox {
        margin: 0 !important;
        padding: 0 !important;
        display: flex !important;
        align-items: center !important;
        width: 100%;
      }

      /* 2. Force the label into a single Flexbox row */
      .header-cell .checkbox label {
        display: flex !important;
        align-items: center !important;
        margin: 0 !important;
        padding: 0 !important;
        font-weight: normal !important;
        cursor: pointer;
      }

      /* 3. Strip absolute positioning from the checkbox to stop overlap */
      .header-cell .checkbox input[type='checkbox'] {
        position: static !important;    
        margin: 0 10px 1px 0 !important;  
        width: 16px !important;
        height: 16px !important;
        flex-shrink: 0 !important;
        cursor: pointer;
        accent-color: #00A120;
        
      }

      /* 4. The actual text */
      .header-cell .checkbox label span {
        font-size: 13px !important;
        line-height: 1 !important;
        padding: 0 !important;
        margin: 0 !important;
      }
            
      body, label, .selectize-input, .leaflet-popup-content {
        font-family: 'Roboto', sans-serif;
      }
      
      /* === Fix for Select Inputs in Flex Header === */

      /* 1. Tell the div wrapping the dropdown to take up all remaining space */
      .header-cell .form-group > div {
        flex: 1 1 auto !important; 
        width: 100%;
      }

      /* 2. Force the actual Selectize box to stretch to the edges of its new container */
      .header-cell .selectize-control {
        width: 100% !important;
      }

      /* 3. Make the label sit nicely to the left of the dropdown */
      .header-cell .form-group > label {
        margin-right: 10px !important;
        margin-bottom: 0 !important; /* Removes Bootstrap's default bottom margin */
        white-space: nowrap !important;
      }
      
      /* === Responsive Screen Block Overlay === */
#responsive-screen-blocker {
  display: none; /* Hidden by default */
  position: fixed;
  top: 0;
  left: 0;
  width: 100vw;
  height: 100vh;
  background-color: rgba(248, 250, 252, 0.98); /* Tailwinds slate-50 canvas color */
  z-index: 999999; /* Ensure it floats above Selectize elements and Leaflet tiles */
  align-items: center;
  justify-content: center;
  padding: 40px;
  box-sizing: border-box;
}

.blocker-content {
  text-align: center;
  max-width: 500px;
}

.blocker-title {
  font-size: 24px;
  font-weight: 700;
  color: #1e293b;
  margin-bottom: 12px;
}

.blocker-text {
  font-size: 15px;
  color: #64748b;
  line-height: 1.6;
}

/* Activate overlay and freeze body scroll when viewport falls under structural limit */
@media screen and (max-width: 1100px) {
  #responsive-screen-blocker {
    display: flex !important;
  }
  body {
    overflow: hidden !important;
  }
}
    ")),
##*HTML Script Tags---- 
    tags$script(HTML("
      /* ── Floating Label State Tracking Logic ────────────────────────── */
      function updateSelectLabels() {
        $('.input-block .form-group').each(function() {
          var hasText = false;
          $(this).find('.selectize-input .item').each(function() {
            if ($(this).text().trim().length > 0) {
              hasText = true;
            }
          });
          
          if (hasText) {
            $(this).addClass('is-filled');
          } else {
            $(this).removeClass('is-filled');
          }
        });
      }

      // Track selection modifications & initialization cycles
      $(document).on('change', '.input-block select', function() { setTimeout(updateSelectLabels, 50); });
      $(document).on('shiny:idle', function() { updateSelectLabels(); });

      /* ── Resize maps to fill remaining vertical space ───────────────── */
      function resizeMaps() {
        var header = document.getElementById('app-header');
        if (!header) return;
        var used      = header.getBoundingClientRect().bottom;
        var remaining = window.innerHeight - used - 5;
        if (remaining < 100) return;
        ['mapA', 'mapB'].forEach(function(id) {
          var el = document.getElementById(id);
          if (el) el.style.height = remaining + 'px';
        });
        setTimeout(function() {
          ['mapA', 'mapB'].forEach(function(id) {
            var mapEl = document.querySelector('#' + id + ' .leaflet-container');
            if (mapEl && mapEl._leaflet_map) mapEl._leaflet_map.invalidateSize();
          });
        }, 50);
      }
      window.addEventListener('resize', resizeMaps);
      $(document).on('shiny:connected', function() {
        var header = document.getElementById('app-header');
        if (header && window.ResizeObserver) {
          new ResizeObserver(resizeMaps).observe(header);
        }
        resizeMaps();
      });

      /* ── Show / hide Region controls when Show Region checkbox changes ──────── */
      $(document).on('change', '#active_region', function() {
        var repoContainer   = document.getElementById('repo-container');
        var catContainer    = document.getElementById('region-cat-container');
        var refineContainer = document.getElementById('region-refine-container');
        var repoCheckbox    = document.querySelector('#repo');

        if (this.checked) {
          repoContainer.style.opacity = '1';
          repoContainer.style.pointerEvents = 'auto';
          repoCheckbox.disabled = false;
          
          catContainer.style.opacity = '1';
          catContainer.style.pointerEvents = 'auto';
          
          refineContainer.style.opacity = '1';
          refineContainer.style.pointerEvents = 'auto';
        } else {
          repoContainer.style.opacity = '0.4';
          repoContainer.style.pointerEvents = 'none';
          repoCheckbox.disabled = true;
          
          catContainer.style.opacity = '0.4';
          catContainer.style.pointerEvents = 'none';
          
          refineContainer.style.opacity = '0.4';
          refineContainer.style.pointerEvents = 'none';
        }
      });

      /* ── Show / hide Civic Infras controls when Show Civic Infras checkbox changes ── */
      $(document).on('change', '#active_infras', function() {
        var infrasCatContainer = document.getElementById('infras-cat-container');

        if (this.checked) {
          infrasCatContainer.style.opacity = '1';
          infrasCatContainer.style.pointerEvents = 'auto';
        } else {
          infrasCatContainer.style.opacity = '0.4';
          infrasCatContainer.style.pointerEvents = 'none';
        }
      });

      /* ── Show / hide Map B when Map Compare checkbox changes ────────── */
      $(document).on('change', '#map_compare', function() {
        var mapA_col      = document.getElementById('mapA-col');
        var mapB_col      = document.getElementById('mapB-col');
        var mapA_controls = document.getElementById('mapA-controls');
        var mapB_controls = document.getElementById('mapB-controls');
        var syncContainer = document.getElementById('sync-move-container');
        var syncCheckbox  = document.querySelector('#sync_move');

        if (this.checked) {
          mapB_col.style.display              = 'block';
          mapB_controls.style.display         = 'flex';
          mapA_col.style.borderRight          = '1px solid #ccc';
          mapA_controls.style.borderRight     = '1px solid #ccc';
          mapA_controls.style.justifyContent  = 'center';
          syncContainer.style.opacity          = '1';
          syncCheckbox.disabled               = false;
        } else {
          mapB_col.style.display              = 'none';
          mapB_controls.style.display         = 'none';
          mapA_col.style.borderRight          = 'none';
          mapA_controls.style.borderRight     = 'none';
          mapA_controls.style.justifyContent  = 'center';
          syncContainer.style.opacity          = '0.4';
          syncCheckbox.disabled               = true;
        }
        setTimeout(function() {
          resizeMaps();
          window.dispatchEvent(new Event('resize'));
        }, 150);
      });

      /* ── Circle-marker hover: grey outline on mouseover ─────────────── */
      function attachCircleHover(mapId) {
        var container = document.getElementById(mapId);
        if (!container) return;

        container.querySelectorAll('path.leaflet-interactive').forEach(function(path) {
          var sw = path.getAttribute('stroke-width');
          if (sw !== null && parseFloat(sw) > 0) return; 
          if (path._hoverBound) return;                  
          path._hoverBound = true;

          path.addEventListener('mouseover', function() {
            if (!this._origStroke) {
              this._origStroke     = this.getAttribute('stroke')     || 'none';
              this._origStrokeW    = this.getAttribute('stroke-width')   || '0';
              this._origStrokeOpac = this.getAttribute('stroke-opacity') || '1';
            }
            this.setAttribute('stroke',     '#333333');
            this.setAttribute('stroke-width',   '2');
            this.setAttribute('stroke-opacity', '0.4');
          });

          path.addEventListener('mouseout', function() {
            if (this._origStroke !== undefined) {
              this.setAttribute('stroke',         this._origStroke);
              this.setAttribute('stroke-width',   this._origStrokeW);
              this.setAttribute('stroke-opacity', this._origStrokeOpac);
            }
          });
        });
      }

      Shiny.addCustomMessageHandler('attachCircleHover', function(data) {
        setTimeout(function() { attachCircleHover(data.map); }, 300);
      });

      /* ── Close Popups on Map Movement (Self-Correcting) ────────────── */
      function attachPopupCloser() {
        var mapsFound = true;
        ['mapA', 'mapB'].forEach(function(id) {
          // Safely look up the leaflet htmlwidget instance
          var widget = typeof HTMLWidgets !== 'undefined' ? HTMLWidgets.find('#' + id) : null;
          
          if (widget && typeof widget.getMap === 'function') {
            var map = widget.getMap();
            
            // Check if we already bound the listener to avoid double-binding
            if (!map._popupCloserBound) {
              map.on('movestart', function() {
                map.closePopup(); // Instantly close popups on manual pans or sync updates
              });
              map._popupCloserBound = true;
            }
          } else {
            // Map or htmlwidgets framework isn't fully loaded yet
            mapsFound = false;
          }
        });
        
        // If maps weren't ready, retry in 200ms
        if (!mapsFound) {
          setTimeout(attachPopupCloser, 200);
        }
      }

      // Re-run checks whenever Shiny finishes a data flush or rendering cycle
      $(document).on('shiny:idle', function() {
        attachPopupCloser();
      });
      
    /* ── Suppress mobile keyboard on all Selectize dropdowns ─────────── */
function suppressSelectizeKeyboard() {
  $('.selectize-input input').attr('inputmode', 'none');
}

/* ── Suppress mobile keyboard on all Selectize dropdowns ─────────── */
function suppressSelectizeKeyboard() {
  $('.selectize-input input').attr('inputmode', 'none');
}

// Run on Shiny events
$(document).on('shiny:connected shiny:idle', suppressSelectizeKeyboard);

// Pre-empt keyboard BEFORE focus fires (touchstart precedes focus on mobile)
$(document).on('touchstart mousedown', '.selectize-control', function() {
  $(this).find('input').attr('inputmode', 'none');
});

// Catch-all fallback at the moment of focus
$(document).on('focus', '.selectize-input input', function() {
  $(this).attr('inputmode', 'none');
});

// Re-apply after any DOM mutation with a short delay
// (Selectize re-init from updateSelectInput happens slightly after the mutation)
var selectizeObserver = new MutationObserver(function(mutations) {
  var hadAdditions = mutations.some(function(m) { return m.addedNodes.length > 0; });
  if (hadAdditions) setTimeout(suppressSelectizeKeyboard, 40);
});
selectizeObserver.observe(document.body, { childList: true, subtree: true });
    "))
  ),
  
##*Header Row 1----
  div(id = "app-header",
      div(style = "width: 100%; border-bottom: 1px solid #ccc;",
          div(style = "max-width: 1400px; margin: 0 auto;",
              div(
                class = "input-block",
                style = "display:flex; align-items:stretch; padding:0px 5px 0px 10px; min-height: 52px;",
                div(
                  class = "header-cell",
                  style = "flex:1 1 90px; min-width:100px; max-width:100px;",
                  checkboxInput("map_compare", "Map Compare", value = TRUE)
                ),
                div(
                  id    = "sync-move-container",
                  class = "header-cell",
                  style = "flex:1 1 90px; min-width:110px; max-width:110px;",
                  checkboxInput("sync_move", "Sync Movement", value = TRUE)
                ),
                
                div(style = "width: 1px; border-left: 1px solid #ccc; margin: 0 6px; align-self: stretch;"),
                
                div(
                  class = "header-cell",
                  style = "flex:1 1 90px; min-width:100px; max-width:100px;",
                  checkboxInput("active_region", "Show Region", value = FALSE)
                ),
                div(
                  id = "repo-container",
                  class = "header-cell",
                  style = "flex:1 1 90px; min-width:100px; max-width:100px; opacity: 0.4; pointer-events: none;",
                  checkboxInput("repo", "Auto Reposition", value = FALSE)
                ),
                div(
                  id = "region-cat-container",
                  class = "header-cell",
                  style = "flex:2 1 240px; min-width:190px; max-width:270px; opacity: 0.4; pointer-events: none;",
                  selectInput("region_category", "Region Category", choices = region_categories, width = "100%")
                ),
                div(
                  id = "region-refine-container",
                  class = "header-cell",
                  style = "flex:2 1 270px; min-width:160px; max-width:400px;opacity: 0.4; pointer-events: none;",
                  selectInput("overlay_refine", "Region Refine", choices = c(""), width = "100%")
                ),

                div(style = "width: 1px; border-left: 1px solid #ccc; margin: 0 6px; align-self: stretch;"),

                div(
                  class = "header-cell",
                  style = "flex:1 1 120px; min-width:120px; max-width:120px; ",
                  checkboxInput("active_infras", "Show Civic Infrastructure", value = FALSE)
                ),
                div(
                  id = "infras-cat-container",
                  class = "header-cell",
                  style = "flex:2 1 240px; min-width:210px; max-width:320px; opacity: 0.4; pointer-events: none;",
                  selectInput("infras_category", "Civic Infrastructure Category", choices = transit_categories, width = "100%")
                ))
          )),
      
##*Header Row 2----
      div(
        style = "display:flex; border-bottom:1px solid #ccc;",
  
        div(
          id    = "mapA-controls",
          class = "input-block",
          style = "flex:1 1 0%; min-width:450px; display:flex; align-items:center; justify-content:center; gap:10px; padding:5px 12px; border-right:1px solid #ccc;",
          div(style = "flex:5 1 160px; min-width:120px; max-width:220px;", selectInput("A_category", "Dataset", choices = category_choices, width = "100%")),
          div(style = "flex:5 1 200px; min-width:140px; max-width:325px;", selectInput("A_variable", "Variable", choices = "", width = "100%")),
          div(style = "flex:1 1 120px; min-width:120px; max-width:180px;", selectInput("A_metric_type", "Normalization", choices = "", width = "100%")),
          div(style = "flex:1 0 80px; min-width:80px; max-width:85px;", selectInput("A_year", "Year", choices = "", width = "100%")),
          div(style = "flex:0 0 30px; display:flex; align-items:center; justify-content:center;",
              actionLink("open_modal_a", "\u24d8", style = "font-size:32px; color:#0055FF; margin-top:2px;", title = "Variable info"))
        ),
        
        div(
          id    = "mapB-controls",
          class = "input-block",
          style = "flex:1 1 0%; min-width:450px; display:flex; align-items:center; justify-content:center; gap:10px; padding:5px 12px;",
          div(style = "flex:2 1 160px; min-width:120px; max-width:220px;", selectInput("B_category", "Dataset", choices = category_choices, width = "100%")),
          div(style = "flex:2.5 1 200px; min-width:140px; max-width:325px;", selectInput("B_variable", "Variable", choices = "", width = "100%")),
          div(style = "flex:1 1 120px; min-width:120px; max-width:160px;", selectInput("B_metric_type", "Normalization", choices = "", width = "100%")),
          div(style = "flex:1 0 80px; min-width:80px; max-width:85px;", selectInput("B_year", "Year", choices = "", width = "100%")),
          div(style = "flex:0 0 30px; display:flex; align-items:center; justify-content:center;",
              actionLink("open_modal_b", "\u24d8", style = "font-size:32px; color:#0055FF; margin-top:2px;", title = "Variable info"))
        )
      )
  ),
  
##*Choropleths----
  div(
    style = "display:flex; margin:0; flex:1; width:100%;",
    div(id = "mapA-col",
        style = "flex:1 1 0%; padding:0; border-right:1px solid #ccc; min-width:450px;",
        leafletOutput("mapA")),
    div(id = "mapB-col",
        style = "flex:1 1 0%; padding:0; min-width:450px;",
        leafletOutput("mapB"))
  )
)

#______________________----
#SERVER----
server <- function(input, output, session) {
  
##*Throttle Guard--------------------------
#Stops the map updates from looping rapidly back and forth between A and B when
#syncing map position
  move_times  <- reactiveVal(numeric(0))
  sync_frozen <- reactiveVal(FALSE)
  
  check_throttle <- function() {
    now   <- as.numeric(Sys.time())
    times <- c(move_times(), now)
    times <- times[times > now - 0.5] 
    move_times(times)
    
    if (length(times) > 3) { #If more than 3 movements register in 0.5s, freeze sync briefly
      sync_frozen(TRUE)
      later::later(function() { 
        sync_frozen(FALSE)
        move_times(numeric(0)) 
      }, delay = 0.15) #Cooldown befre freezing again
      return(TRUE)
    }
    return(FALSE)
  }
  
  sync_source <- reactiveVal("none")
  
##*helper functions----
  format_geo_label <- function(x) {
    x <- gsub("^geo_", "", x)
    x <- gsub("_",     " ", x)
    toTitleCase(x)
  }
  
  trigger_hover_bind <- function(map_id) {
    session$sendCustomMessage("attachCircleHover", list(map = map_id))
  }
  
##*Metadata Popup----
  observeEvent(input$open_modal_a, {
    info <- var_config[var_config$base_var == input$A_variable &
                         var_config$metric_type == input$A_metric_type, ]
    if (nrow(info) == 0) return()
    showModal(modalDialog(
      title     = h3(strong(info$category)),
      if (!is.na(info$geo_df))       p(strong("Base Geometry: "),  format_geo_label(info$geo_df)),
      if (!is.na(info$last_updated)) p(strong("Last Updated: "),   info$last_updated),
      if (!is.na(info$description))  p(strong("Description: "),    info$description),
      if (!is.na(info$url))          a("Link to Source", href = info$url, target = "_blank"),
      size      = "m",
      easyClose = TRUE,
      fade      = FALSE,
      footer    = modalButton("Return to Map")
    ))
  })
  
  observeEvent(input$open_modal_b, {
    info <- var_config[var_config$base_var == input$B_variable &
                         var_config$metric_type == input$B_metric_type, ]
    if (nrow(info) == 0) return()
    showModal(modalDialog(
      title     = h3(strong(info$category)),
      if (!is.na(info$geo_df))       p(strong("Base Geometry: "),  format_geo_label(info$geo_df)),
      if (!is.na(info$last_updated)) p(strong("Last Updated: "),   info$last_updated),
      if (!is.na(info$description))  p(strong("Description: "),    info$description),
      if (!is.na(info$url))          a("Link to Source", href = info$url, target = "_blank"),
      size      = "m",
      easyClose = TRUE,
      fade      = FALSE,
      footer    = modalButton("Return to Map")
    ))
  })
  
##*Region Input Options----
  update_refine_choices <- function() {
    req(input$region_category)
    updateSelectInput(session, "overlay_refine",
                      choices = if (input$region_category == "City of Phl. Council Districts") {
                        district_levels
                      } else {
                        sort(subset(overlay_region, category == input$region_category)$refine)
                      })
  }
  observeEvent(input$region_category, update_refine_choices())
  
##*Map User Input Options----
  update_variable_choices <- function(side) {
    cat_input   <- paste0(side, "_category")
    var_input   <- paste0(side, "_variable")
    current_var <- input[[var_input]]
    root        <- var_config$base_var[var_config$var == current_var]
    vars        <- subset(var_config, category == input[[cat_input]])
    vars        <- vars[!duplicated(vars$base_var), ]
    
    raw_choices <- setNames(vars$base_var, as.character(vars$label))
    sorted_choices <- raw_choices[order(tolower(trimws(names(raw_choices))))]
    
    selected    <- if (length(root) > 0 && root %in% sorted_choices) root else NULL
    
    updateSelectInput(session, var_input,
                      choices  = sorted_choices,
                      selected = selected)
  }
  observeEvent(input$A_category,     update_variable_choices("A"))
  observeEvent(input$B_category,     update_variable_choices("B"))
  observeEvent(input$A_metric_type,  update_variable_choices("A"))
  observeEvent(input$B_metric_type,  update_variable_choices("B"))
  
  update_type_choices <- function(side) {
    cat_input    <- paste0(side, "_category")
    var_input    <- paste0(side, "_variable")
    type_input   <- paste0(side, "_metric_type")
    current_type <- input[[type_input]]
    req(input[[cat_input]], input[[var_input]])
    root         <- var_config$base_var[var_config$var == input[[var_input]]]
    types        <- subset(var_config, category == input[[cat_input]] & base_var == root)
    selected_type <- if (!is.null(current_type) && current_type %in% types$metric_type) current_type else NULL
    updateSelectInput(session, type_input,
                      choices  = unique(types$metric_type),
                      selected = selected_type)
  }
  observeEvent(input$A_category,  update_type_choices("A"))
  observeEvent(input$B_category,  update_type_choices("B"))
  observeEvent(input$A_variable,  update_type_choices("A"))
  observeEvent(input$B_variable,  update_type_choices("B"))

  update_year_choices <- function(side) {
    var_input  <- paste0(side, "_variable")
    type_input <- paste0(side, "_metric_type")
    year_input <- paste0(side, "_year")
    req(input[[var_input]], input[[type_input]])
    row      <- subset(var_config, base_var == input[[var_input]] & metric_type == input[[type_input]])
    yrs      <- sort(unique(as.character(unlist(row$years))))
    current <- input[[year_input]]
    selected <- if (!is.null(current) && current %in% yrs) current else if (length(yrs)) tail(yrs, 1) else character(0)
    updateSelectInput(session, year_input, choices = yrs, selected = selected)
  }
  observeEvent(input$A_variable,    update_year_choices("A"))
  observeEvent(input$A_metric_type, update_year_choices("A"))
  observeEvent(input$B_variable,    update_year_choices("B"))
  observeEvent(input$B_metric_type, update_year_choices("B"))
  
##*Update Overlays----
  redraw_overlays <- function(map_id) {
    proxy <- leafletProxy(map_id)
    
    proxy %>% 
      clearGroup("REGION") %>% 
      clearGroup("INFRAS") %>% 
      removeControl("infras_legend")
    
###Draw Civic Infrastructure----
    if (isTRUE(input$active_infras)) {

      selected_infras <- if (input$infras_category == "Rail: All") {
        overlay_civic_infras %>% filter(str_detect(category, "^Rail:"))
      } else if (input$infras_category == "Schools by Type: All") {
        overlay_civic_infras %>%
          filter(str_detect(category, "^Schools by Type:")) %>%
          distinct(name, .keep_all = TRUE)
      } else if (input$infras_category == "Schools by Grade: All") {
        overlay_civic_infras %>%
          filter(str_detect(category, "^Schools by Grade:")) %>%
          distinct(name, .keep_all = TRUE)
      } else if (!str_detect(input$infras_category, ":")) {
        
        overlay_civic_infras %>% filter(category == input$infras_category)
        
      } else {
        prefix_group <- str_extract(input$infras_category, "^[^:]+:\\s*")
        search_term  <- str_remove(input$infras_category, "^.*:\\s*")
        
        overlay_civic_infras %>% 
          filter(
            str_detect(category, fixed(prefix_group)) & 
              str_detect(category, fixed(search_term))
          )
      }
        
        overlay_civic_infras %>% filter(category == input$infras_category)
      
      # Map Geometry Routing
      geom_types <- unique(sf::st_geometry_type(selected_infras))
      
      if (any(geom_types %in% c("POINT", "MULTIPOINT"))) {
        proxy %>%
          addCircleMarkers(
            data        = selected_infras,
            group       = "INFRAS",
            fillColor   = ~color,
            fillOpacity = 0.5,
            weight      = 0,
            radius      = 9,
            options     = pathOptions(interactive = TRUE),
            popup       = ~paste0("<b>Name: </b>", name, "<br>", size),
            popupOptions = popupOptions(autoClose = TRUE, closeOnClick = TRUE, closeOnMove = FALSE)
          )
        trigger_hover_bind(map_id)
        
      } else {
        proxy %>%
          addPolylines(
            data    = selected_infras, 
            group   = "INFRAS",
            color   = ~color, 
            opacity = 1, 
            weight  = 3,
            options = pathOptions(interactive = FALSE)
          )
      }
      
      unique_categories <- unique(selected_infras$category)
      
      if (length(unique_categories) > 1) {
        legend_df <- selected_infras %>%
          sf::st_drop_geometry() %>% 
          distinct(category, color)
        
        if (is.factor(legend_df$category)) {
          legend_df <- legend_df %>% arrange(as.numeric(category))
        } else {
          legend_df <- legend_df %>% arrange(category)
        }
        
        # Clean prefix strings dynamically for the labels
        clean_labels <- str_remove(legend_df$category, "^.*:\\s*")
        
        # Determine clean titles contextually based on the active category string
        legend_title <- case_when(
          str_detect(input$infras_category, "^Rail:")              ~ "Rail Type",
          str_detect(input$infras_category, "^Schools by Type:")   ~ "School Type",
          str_detect(input$infras_category, "^Schools by Grade:")  ~ "School Grade",
          TRUE                                                     ~ "Infrastructure"
        )
        
        proxy %>%
          addLegend(
            layerId  = "infras_legend",  
            position = "bottomleft",
            colors   = legend_df$color,
            labels   = clean_labels,
            title    = legend_title,
            opacity  = 0.7
          )
      }
    }
    
    ###Draw Regional Boundaries----
    if (isTRUE(input$active_region)) {
      selected_region <- overlay_region %>%
        filter(category == input$region_category, refine == input$overlay_refine)
      
      proxy %>%
        addPolygons(
          data        = selected_region, 
          group       = "REGION",
          fillOpacity = 0, 
          color       = "orange",
          opacity     = 0.8, 
          weight      = 3,
          options     = pathOptions(interactive = FALSE)
        )
    }
  }

  observeEvent(list(input$active_region, input$active_infras,
                    input$region_category, input$overlay_refine,
                    input$infras_category), {
                      redraw_overlays("mapA")
                      redraw_overlays("mapB")
                    })
  
  ##*Update Choropleth----
  update_map <- function(map_id, base_variable, metric_type, selected_year) {
    req(base_variable, metric_type, selected_year)
    
    info <- var_config[var_config$base_var == base_variable &
                         var_config$metric_type == metric_type, ]
    req(nrow(info) > 0)
    info <- info[1, ]
    
    geo_data      <- get(info$geo_df)
    var_data_name <- gsub("geo_", "app_", info$geo_df)
    var_data      <- get(var_data_name) %>% filter(year == as.numeric(selected_year))
    map_data      <- geo_data %>%
      inner_join(var_data %>% filter(!is.na(.data[[info$var]])), by = "region_id")
    
    vals          <- suppressWarnings(as.numeric(map_data[[info$var]]))
    region        <- format_geo_label(info$geo_df)
    pal           <- colorNumeric(info$palette, domain = c(info$range_min, info$range_max))
    fmt           <- formatters[[ info$formatter ]]
    formatted_val <- fmt(vals)
    
    leafletProxy(map_id, data = map_data) %>%
      clearGroup("BASE") %>%
      clearControls() %>%
      addPolygons(
        fillColor   = pal(vals),
        fillOpacity = 0.6,
        color       = "transparent",
        weight      = 1,
        popup       = paste0("<b>", region, ": </b>",
                             sub("^.*\\|", "", map_data$region_id),
                             "<br><b>", info$label, ":</b> ", formatted_val),
        popupOptions = popupOptions(
          autoClose    = TRUE,
          closeOnClick = TRUE,
          closeOnMove = FALSE
        ),
        group       = "BASE",
        highlightOptions = highlightOptions(
          weight      = 2,
          color       = "#333333",
          opacity     = 0.4,
          bringToFront = FALSE
        )
      ) %>%
      addLegend(
        "bottomright", pal = pal,
        values = c(info$range_min, info$range_max),
        title  = HTML(paste0(
          "<span style='display:block; width:110px;
           white-space:normal; word-wrap:break-word;'>",
          info$label, "</span>"
        )),
        labFormat = if (info$formatter == "percent") {
          labelFormat(suffix = "%")
        } else if (info$formatter == "dollar") {
          labelFormat(prefix = "$", big.mark = ",")
        } else {
          labelFormat()
        },
        opacity = 0.6
      ) %>%
      addControl(
        html = paste0(
          "<div style='font-size:14px; width:110px;
           box-sizing:border-box; word-wrap:break-word;'>
           <b>Region:</b> ", info$regions_covered, "</div>"
        ),
        position = "bottomright"
      )
    
    redraw_overlays(map_id)
  }
  
  observeEvent(list(input$A_variable, input$A_metric_type, input$A_year), {
    req(input$A_variable, input$A_metric_type, input$A_year)
    update_map("mapA", input$A_variable, input$A_metric_type, input$A_year)
  })
  
  observeEvent(list(input$B_variable, input$B_metric_type, input$B_year), {
    req(input$B_variable, input$B_metric_type, input$B_year)
    update_map("mapB", input$B_variable, input$B_metric_type, input$B_year)
  })
  
##*Base Map----
  base_map <- function() {
    leaflet(options = leafletOptions(maxZoom = 16)) %>%
      addProviderTiles("Esri.WorldGrayCanvas",
                       options = tileOptions(
                         updateWhenIdle    = TRUE,
                         updateWhenZooming = FALSE,
                         keepBuffer        = 4
                       )) %>%
      setView(lng = -75.12, lat = 40, zoom = 11)
  }
  
  output$mapA <- renderLeaflet(base_map())
  output$mapB <- renderLeaflet(base_map())
  
##*Sync Map Position----
  observeEvent(input$mapA_bounds, {
    if (sync_frozen()) return()
    if (isFALSE(input$sync_move)) return()
    if (sync_source() == "REPO") { sync_source("none"); return() }
    if (sync_source() == "B") return()
    
    if (check_throttle()) return()
    
    sync_source("A")
    leafletProxy("mapB") %>%
      setView(input$mapA_center$lng, input$mapA_center$lat, input$mapA_zoom,
              options = list(animate = FALSE))
    
    later::later(function() { sync_source("none") }, delay = 0.05)
  })
  
  observeEvent(input$mapB_bounds, {
    if (sync_frozen()) return()
    if (isFALSE(input$sync_move)) return()
    if (sync_source() == "REPO") { sync_source("none"); return() }
    if (sync_source() == "A") return()
    
    if (check_throttle()) return()
    
    sync_source("B")
    leafletProxy("mapA") %>%
      setView(input$mapB_center$lng, input$mapB_center$lat, input$mapB_zoom,
              options = list(animate = FALSE))
    
    later::later(function() { sync_source("none") }, delay = 0.05)
  })
  
##*Region Auto Reposition----
  observeEvent(list(input$overlay_refine, input$repo, input$active_region), {
    req(isTRUE(input$active_region), isTRUE(input$repo), input$overlay_refine)
    selected_region <- overlay_region %>%
      filter(category == input$region_category, refine == input$overlay_refine)
    bounds <- st_bbox(selected_region)
    sync_source("REPO")
    leafletProxy("mapA") %>% fitBounds(bounds[[1]], bounds[[2]], bounds[[3]], bounds[[4]])
    leafletProxy("mapB") %>% fitBounds(bounds[[1]], bounds[[2]], bounds[[3]], bounds[[4]])
  })
}

shinyApp(ui = ui, server = server)


#things to add
#download png of graph
#download processed dataset