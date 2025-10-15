% Class requires LuaLatex
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{estat}[2024-02-24 Standard estat class]
% \ProvidesExplClass {estat} {2024/03/09} {0.1} {Standard estat class}

% \DeclareOption*{\PassOptionsToClass{\CurrentOption{article}}}
\DeclareOption*{\ClassWarning{estat}{\CurrentOption not declared!}}
\ProcessOptions\relax

% Select article as main class
\LoadClass[12pt]{article} 
% Setup margin and paper size
\RequirePackage[a4paper, top=3cm, bottom=2cm, left=3cm, right=2cm]{geometry} 

% Set language to PT-BR
%\RequirePackage[brazil]{babel}




% Packages for math formats
\RequirePackage{amsmath} 
\RequirePackage{amssymb}
\RequirePackage{amsfonts}
\RequirePackage{amsthm}

% First paragraph indentition
\RequirePackage{indentfirst}
% Better floating boxes manipulation
\RequirePackage{float} 

% Multi Column and Multi Row cells on tables
\RequirePackage{multicol} 
\RequirePackage{multirow} 

% Image Package
\RequirePackage{graphicx}

% Permits any font size, not only the ones set on font document
\RequirePackage{anyfontsize} 

% Set custom spacing between lines 
\RequirePackage{setspace} % TODO : Determine if this is realy necessarie


% Use True Arial font
\RequirePackage{fontspec}
\setmainfont{Arial}
% \setmonofont{Fira Code}[
%   Contextuals=Alternate  % Activate the calt feature
% ]

% Bibliographical Referencing 
\RequirePackage[resetlabels, labeled]{multibib} 

% Package to create new Float environmetns. Used to create the "quadro" float
\RequirePackage{newfloat} 
% \DeclareFloatingEnvironment[listname=loq, listname={Lista de Quadros}]{quadro}

% Package to change tables spacing 
\RequirePackage{tabls} % TODO : Determine if this is realy necessarie
\setlength{\tablinesep}{5pt} 

% Package used to calculate inside function parameters
\RequirePackage{calc} 

% hyper-link and referencing
\RequirePackage{hyperref}
\hypersetup{%
    allcolors=black,%
    colorlinks=true,%
}

% Package to create better table formats
\RequirePackage{booktabs}

% Package to better fit images and tables withn margins
\RequirePackage[export]{adjustbox}

% For spacing at the end of commands
\RequirePackage{xspace}

% \RequirePackage{fancyhdr} % Not really necessary

\RequirePackage{xcolor}

% estat colors
\definecolor{estatRed}{HTML}{A11D21}
\definecolor{estat1}{HTML}{A11D21}
\definecolor{estat2}{HTML}{003366}
\definecolor{estat3}{HTML}{CC9900}
\definecolor{estat4}{HTML}{663333}
\definecolor{estat5}{HTML}{FF6600}
\definecolor{estat6}{HTML}{CC9966}
\definecolor{estat7}{HTML}{999966}
\definecolor{estat8}{HTML}{006606}
\definecolor{estat9}{HTML}{008091}
\definecolor{estat10}{HTML}{041835}
\definecolor{estat11}{HTML}{666666}

% listings colors
\definecolor{black}{RGB}{0, 0, 0}
\definecolor{backgroundCol}{rgb}{0.95, 0.95, 0.95}
\definecolor{commentstyleCol}{rgb}{0.678, 0.584, 0.686}
\definecolor{keywordstyleCol}{rgb}{0.737, 0.353, 0.396}
\definecolor{stringstyleCol}{rgb}{0.192, 0.494, 0.8}
\definecolor{NumCol}{rgb}{0.686, 0.059, 0.569}
\definecolor{basicstyleCol}{rgb}{0.345, 0.345, 0.345}   


\RequirePackage{listings}
\lstset{ 
  language=R,                     % the language of the code
  basicstyle=\small  \ttfamily \color{basicstyleCol}, % the size of the fonts that are used for the code
%  numbers=left,                   % where to put the line-numbers
%  numberstyle=\color{green},  % the style that is used for the line-numbers
  stepnumber=1,                   % the step between two line-numbers. If it is 1, each line
                                  % will be numbered
  numbersep=5pt,                  % how far the line-numbers are from the code
  backgroundcolor=\color{backgroundCol},  % choose the background color. You must add \usepackage{color}
  showspaces=false,               % show spaces adding particular underscores
  showstringspaces=false,         % underline spaces within strings
  showtabs=false,                 % show tabs within strings adding particular underscores
  %frame=single,                   % adds a frame around the code
  %rulecolor=\color{white},        % if not set, the frame-color may be changed on line-breaks within not-black text (e.g. commens (green here))
  tabsize=2,                      % sets default tabsize to 2 spaces
  captionpos=b,                   % sets the caption-position to bottom
  breaklines=true,                % sets automatic line breaking
  breakatwhitespace=false,        % sets if automatic breaks should only happen at whitespace
  keywordstyle=\color{keywordstyleCol},      % keyword style
  commentstyle=\color{commentstyleCol},   % comment style
  stringstyle=\color{stringstyleCol},      % string literal style
  frame=shadowbox,
  literate=%
   *{0}{{{\color{NumCol}0}}}1
    {1}{{{\color{NumCol}1}}}1
    {2}{{{\color{NumCol}2}}}1
    {3}{{{\color{NumCol}3}}}1
    {4}{{{\color{NumCol}4}}}1
    {5}{{{\color{NumCol}5}}}1
    {6}{{{\color{NumCol}6}}}1
    {7}{{{\color{NumCol}7}}}1
    {8}{{{\color{NumCol}8}}}1
    {9}{{{\color{NumCol}9}}}1, 
}

\RequirePackage{tikz}
\usetikzlibrary {%
    arrows.meta,
    graphs,
    shapes,
    positioning,
    decorations.pathmorphing,
    calc,
    backgrounds, 
    mindmap, 
    shadows,
    fadings, 
    patterns,
    fit,
    % pgffor,
}

\RequirePackage{array}
\newcolumntype{P}[1]{>{\centering\arraybackslash}p{#1}}
\newcolumntype{M}[1]{>{\centering\arraybackslash}m{#1}}

\RequirePackage{siunitx}
\sisetup{
mode = match,
round-mode = places,
round-minimum = 0,
round-precision = 2,
round-zero-positive = true,
group-digits = integer,
group-separator = {.},
output-decimal-marker = {,},
table-alignment-mode = format,
table-number-alignment = center,
table-format = 2.2,
table-auto-round = true,
list-final-separator = { \GetTranslation{and} },
list-pair-separator = { \GetTranslation{and} },
range-phrase = { \GetTranslation{to (numerical range)} },
}
%\selectlanguage{brazilian}
\newcommand{\inteiro}[1]{\num[drop-zero-decimal]{#1}}
\newcommand{\pvalor}[1]{\num[round-precision = 3, round-minimum = 0.001]{#1}}
\newcolumntype{P}[0]{S<{~\unit{\percent}}}
\newcolumntype{N}[0]{S[drop-zero-decimal, group-digits = integer, table-number-alignment = right]}

\RequirePackage{framed} %%%%%%%%%%s%%%%% for debugging


\AddToHook{begindocument/before} {
    \onehalfspacing
    \pagestyle{empty}
    \pagenumbering{arabic}
}

\newcommand{\offset}{-0.3em}

\AddToHook{begindocument/before} {
    \onehalfspacing
}

\AddToHook{cmd/section/before} {
    \newpage
}






\renewcommand{\maketitle}{%
% Title page background
\AddToHookNext{shipout/background}{%
    \put(0, 0){\makebox(0, 0)[tl]{\includegraphics[width=\paperwidth,height=\paperheight]{estat/imagens/capa.png}}}
}

\begin{tikzpicture}[remember picture, overlay]
    \begin{scope}[execute at begin scope={\linespread{3}}]
    \draw (current page.north) 
    node[
        text width=0.9\textwidth,
        minimum height = 6cm,
        align=flush left,
        ] 
        at +(1cm, -8.9cm)
        {{\fontsize{38}{0}\selectfont\@title}};%
    \end{scope}

    \begin{scope}[node distance=0.5cm, text width= 6cm, align=flush left, anchor=north east]
        \draw (0,0) ++(\textwidth,0) ++(-1cm,-10cm) 
        node(exec) {\textbf{Consultores Responsáveis:}\\ \@authors}
        node[below= of exec] {\textbf{Requerente:}\\ \@client};
    \end{scope}

    \begin{scope}[node distance=0.25cm]
        \draw (current page.south) node(logo)[anchor=south] at +(0, 3cm){\includegraphics[height=1.5cm]{estat/imagens/estat.png}};
        \node(date)[above= of logo, align=flush center]{Brasília, \today.};
    \end{scope}
\end{tikzpicture}

\newpage
}

% ALTERANDO O SUMARIO
\makeatletter
\renewcommand{\tableofcontents}{%
    \null\hfill\textbf{\Large\contentsname}\hfill\null\par%
    \@mkboth{\MakeUppercase\contentsname}{\MakeUppercase\contentsname}%
    \@starttoc{toc}%
}
\renewcommand{\l@section}{\@dottedtocline{1}{1.5em}{2.3em}}
\renewcommand{\l@subsection}{\@dottedtocline{2}{3.8em}{3.2em}}
\renewcommand{\l@subsubsection}{\@dottedtocline{3}{7.0em}{4.1em}}
\makeatother	

\addtocontents{toc}{%
    ~\hfill\textbf{Página}\par%
}

% \addto\captionsportuguese{%
%   \renewcommand{\contentsname}{Sumário}
%   \renewcommand{\tablename}{Tabela}
%   \renewcommand{\figurename}{Figura}
% }

% Default page background
\AddToHook{shipout/background}{
    \put(0, 0){\makebox(0, 0)[tl]{\includegraphics[width=\paperwidth,height=\paperheight]{estat/imagens/pagina-comum.png}}}
}



% LOGO DA ESTAT AQUI NAO FUNCIONA
\makeatletter
\newcommand{\ps@default}{%
    % numero da pagina com cor clara e no canto
    \renewcommand{\@oddfoot}{\textcolor{white}{\null\hfill\thepage}}%
    % \renewcommand{\@evenfoot}{\thepage}%
    % definindo header para o canto de cima
    % \renewcommand{\@evenhead}{\null\hfil\slshape\leftmark}%
    \renewcommand{\@oddhead}{{\slshape\hfill \includegraphics[scale=0.2]{estat/imagens/estat.png}}}%
}
\makeatother














\makeatletter
\newcommand{\@client}{}
\newcommand{\client}[1]{\renewcommand{\@client}{#1}}
\makeatother

\makeatletter
\newcommand{\@authors}{}
\newcommand{\authors}[1]{\renewcommand{\@authors}{#1}}
\makeatother


\newcolumntype{I}[0]{>{\centering\arraybackslash}m{7cm}}
\newcommand{\interval}[2]{% código que gera intervalo
    % \begin{tikzpicture}[>={Stealth[round]}, thick, label distance=2mm]
    \begin{tikzpicture}[>={Stealth[round, scale=1.25]}, very thick, label distance=2mm]
    
    \coordinate[label=below:$#1$] (A) at (-2,0);
    \coordinate[label=below:$#2$] (B) at (2,0); 
    \coordinate (C) at (-3,0);
    \coordinate (D) at (3,0);
    
    \coordinate (corner) at (-3.25, -0.75);
    \coordinate (corner_top) at (3.25, 0.75);

    \node at (corner) {};
    \node at (corner_top) {};

    \draw (C) -- ($(A) - (2mm,0)$) ;
    \draw [->] ($(B) + (2mm,0)$) -- (D);
    % \draw [decorate, decoration=zigzag] (A) -- (B);
    % \draw [decorate, decoration={snake, pre length=2mm, post length=2mm}] (A) ++(2mm,0) -- (B);
    \draw [decorate, decoration={snake, pre length=0mm, post length=0mm}] ($(A) + (2mm,0)$) -- ($(B) - (2mm,0)$);
    \draw ($(A) + (2mm,0)$) -- ($(B) - (2mm,0)$);
    \draw (A) circle[radius=2mm];
    \draw (B) circle[radius=2mm];
    
    \end{tikzpicture}
}

\newcommand{\hipoteses}[2]{%
\par
$$
    \begin{cases}
        H_{0}: \parbox[t]{2\textwidth/3}{#1} \\
        H_{1}: \parbox[t]{2\textwidth/3}{#2} \\
    \end{cases}
$$
\newline
}

\labelformat{table}{\textbf{Tabela #1}}
% \labelformat{quadro}{\textbf{Quadro #1}}
\labelformat{figure}{\textbf{Figura #1}}