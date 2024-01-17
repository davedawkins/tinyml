module AppCss

open Sutil
open Sutil.Styling

open type Feliz.length

let theme = { 
    SutilOxide.Css.LightTheme  with
        ContentBackground = "white" 
        ControlBackground =  "#eef8ff" // "#c5e8ff" //"#f4ffef" // hsl(101,100%,96%)
    } 

let headerColor = "#d3edfd"

let centerContent =  [
    Css.displayFlex
    Css.alignItemsCenter
    Css.justifyContentCenter
]

let style = [

    rule ".thm-control" [
        Css.backgroundColor theme.ControlBackground
    ]

    rule ".thm-content" [
        Css.backgroundColor theme.ContentBackground
        Css.backgroundColor theme.ContentForeground
    ]

    rule ".vertical" [
        Css.displayFlex
        Css.flexDirectionColumn
    ]
    rule ".horizontal" [
        Css.displayFlex
        Css.flexDirectionRow
    ]

    rule ".centre-content" [
        Css.displayFlex
        Css.alignItemsCenter
        Css.justifyContentCenter
    ]

    rule ".container" [
        Css.width (vw 100)
        Css.height (vh 100)
        Css.overflowHidden
    ]

    rule ".pad" [ Css.padding (rem 0.5)]
    rule ".gap" [ Css.gap (rem 0.5)]
    rule ".full-width" [ Css.width (percent 100)]
    rule ".full-height" [ Css.height (percent 100)]

    rule ".error" [
        Css.color "red"
    ]

    rule ".ok" [
        Css.color "green"
    ]

    rule ".small" [
        Css.fontSize (percent 80)
    ]
    
    rule ".left-stack" [
        Css.width (percent 50)
    ]

    rule ".header" [
        Css.backgroundColor theme.ControlBackground
    ]

    rule "button" [
        Css.borderStyleNone
        Css.backgroundColor ""
    ]

    rule ".main" [
        Css.minHeight 0
    ]

    rule ".main-header" [
        Css.justifyContentSpaceBetween
        Css.borderBottomWidth (px 1)
        Css.borderBottomStyle (Feliz.borderStyle.solid)
        Css.borderBottomColor ("#eeeeee")
        Css.backgroundColor headerColor // theme.ControlBackground //"#42d0010a"
    ]

    rule ".header-item" [
        yield! centerContent
        Css.margin (rem 0.2)
    ]

    rule "i.header-item" [
        Css.color ("#122612")
    ]

    rule "button" [
        Css.borderStyleNone
    ]
    
    rule "button.header-item" [
        Css.color ("#122612")
        Css.custom("height", "fit-content")
        Css.backgroundColor "transparent"
        //Css.borderRadius (px 4)
        Css.padding (px 0)
        Css.borderBottom (px 2, Feliz.borderStyle.solid, "transparent")
    ]

    rule "button.header-item:hover" [
        Css.borderBottom (px 2, Feliz.borderStyle.solid, "#122612")
    ]

    rule "button.header-item:active" [
        Css.backgroundColor "#dddddd"        
    ]
]
