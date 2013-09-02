\documentclass{beamer}
%\documentclass[$if(fontsize)$$fontsize$,$endif$$if(handout)$handout,$endif$$if(beamer)$ignorenonframetext,$endif$]{$documentclass$}
\usetheme{Galois}
\usepackage{amssymb,amsmath}
\usepackage{ifxetex,ifluatex}
\usepackage{fixltx2e} % provides \textsubscript
\ifxetex
  \usepackage{fontspec,xltxtra,xunicode}
  \defaultfontfeatures{Mapping=tex-text,Scale=MatchLowercase}
\else
  \ifluatex
    \usepackage{fontspec}
    \defaultfontfeatures{Mapping=tex-text,Scale=MatchLowercase}
  \else
    \usepackage[utf8]{inputenc}
  \fi
\fi
$if(natbib)$
\usepackage{natbib}
\bibliographystyle{plainnat}
$endif$
$if(biblatex)$
\usepackage{biblatex}
$if(biblio-files)$
\bibliography{$biblio-files$}
$endif$
$endif$
$if(listings)$
\usepackage{listings}
\usepackage{microtype}
\usepackage{syntax}
\usepackage{qtree}
$endif$
$if(lhs)$
\lstnewenvironment{code}{\lstset{language=Haskell,basicstyle=\small\ttfamily}}{}
$endif$
$if(highlighting-macros)$
$highlighting-macros$
$endif$
$if(verbatim-in-note)$
\usepackage{fancyvrb}
$endif$
$if(fancy-enums)$
\usepackage{enumerate}
$endif$
$if(tables)$
\usepackage{ctable}
\usepackage{float} % provides the H option for float placement
$endif$
$if(url)$
\usepackage{url}
$endif$
\usepackage{graphicx}
% Comment these out if you don't want a slide with just the
% part/section/subsection/subsubsection title:
\AtBeginPart{\frame{\partpage}}
\AtBeginSection{\frame{\sectionpage}}
\AtBeginSubsection{\frame{\subsectionpage}}
\AtBeginSubsubsection{\frame{\subsubsectionpage}}
$if(strikeout)$
\usepackage[normalem]{ulem}
% avoid problems with \sout in headers with hyperref:
\pdfstringdefDisableCommands{\renewcommand{\sout}{}}
$endif$
\setlength{\parindent}{0pt}
\setlength{\parskip}{6pt plus 2pt minus 1pt}
\setlength{\emergencystretch}{3em}  % prevent overfull lines
$if(numbersections)$
$else$
\setcounter{secnumdepth}{0}
$endif$
$if(verbatim-in-note)$
\VerbatimFootnotes % allows verbatim text in footnotes
$endif$
$if(lang)$
\usepackage[$lang$]{babel}
$endif$
$for(header-includes)$
$header-includes$
$endfor$


$if(title)$
\title{$title$}
$endif$
$if(author)$
\author{$for(author)$$author$$sep$ \and $endfor$}
$endif$
$if(date)$
\date{$date$}
$endif$


\definecolor{javared}{rgb}{0.6,0,0} % for strings
\definecolor{javagreen}{rgb}{0.25,0.5,0.35} % comments
\definecolor{javapurple}{rgb}{0.5,0,0.35} % keywords
\definecolor{javadocblue}{rgb}{0.25,0.35,0.75} % javadoc
 
\newcommand\Small{\fontsize{8}{8.0}\selectfont}
\newcommand*\LSTfont{\Small\ttfamily\SetTracking{encoding=*}{-60}\lsstyle}
\lstset{language=Java,
%basicstyle=\ttfamily,
%% JED: Kill these lines if you want black&white
%%keywordstyle=\color{javapurple}\bfseries,
%%stringstyle=\color{javared},
%%commentstyle=\color{javagreen},
%%morecomment=[s][\color{javadocblue}]{/**}{*/},
%%morecomment=[l][\bfseries\color{javared}]{>-\ },
%%morecomment=[l][\color{javared}]{-\ },
%%morecomment=[l][\color{javagreen}]{+\ },
tabsize=4,
showspaces=false,
showstringspaces=false,
basicstyle=\LSTfont,
%frame=single, 
captionpos=b,
escapeinside={$$}{$$}}

%%\lstnewenvironment{java}[1][]{}{}
%%\lstnewenvironment{java}[1][]
%%{\lstset{#1,
%%         frame=single, 
%%         captionpos=b,
%%         basicstyle=\LSTfont,
%%         escapeinside={$$}{$$}}}
%%%%          basicstyle=\scriptsize\ttfamily}}
%%%%          basicstyle=\footnotesize\ttfamily}}
%%{}

\newcommand{\metavar}{$$\square$$}

\begin{document}

\galoistitle

%$if(title)$
%\frame{\titlepage}
%$endif$

$for(include-before)$
$include-before$

$endfor$
$if(toc)$
\begin{frame}
\tableofcontents[hideallsubsections]
\end{frame}

$endif$
$body$

$if(natbib)$
$if(biblio-files)$
$if(biblio-title)$
$if(book-class)$
\renewcommand\bibname{$biblio-title$}
$else$
\renewcommand\refname{$biblio-title$}
$endif$
$endif$
\bibliography{$biblio-files$}

$endif$
$endif$
$if(biblatex)$
\printbibliography$if(biblio-title)$[title=$biblio-title$]$endif$

$endif$
$for(include-after)$
$include-after$

$endfor$
\end{document}
