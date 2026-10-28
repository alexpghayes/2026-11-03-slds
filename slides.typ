#import "@preview/polylux:0.4.0": *
#import "@preview/cetz:0.3.4": canvas, draw
#import "theme.typ": *
#import "support-code.typ": *

#show: setup

// ---------------------------------------------------------------------------
// Local helpers
// ---------------------------------------------------------------------------

#let R = $bb(R)$
#let diid = $limits(tilde)^"iid"$
// Color convention (all colors come from theme.typ):
//  - dark-teal : default text and structure (graph edges, boxes)
//  - bright (orange) : emphasis / "the thing to look at": treated nodes, problem terms,
//                      and the standard (observed-network, known-A) model
//  - red : our proposal: the latent / estimated-network (smoothed) model
//  - muted (gray) : untreated / background
#let hl(body) = text(fill: bright, body)
#let pc(body) = hl(body) // peer contagion: standard model using the observed network
#let lc(body) = text(fill: red, body) // latent contagion: our proposed model
#let under(body) = underline(offset: 0.15em, body)

// Notation from the CAS poster (orange = peer contagion, red = latent contagion)
#let betanaught = pc($beta_0$)
#let betaw = pc($beta_"w"$)
#let betax = pc($beta_"x"$)
#let betay = pc($beta_"y"$)
#let bbeta = pc($bold(beta)$)
#let thetanaught = lc($theta_0$)
#let thetaw = lc($theta_"w"$)
#let thetax = lc($theta_"x"$)
#let thetay = lc($theta_"y"$)
#let btheta = lc($bold(theta)$)
#let thetahattsls = lc($hat(btheta)$)


// Annotation under/over an underbrace: small, centered, multi-line
#let lbl(..lines) = text(size: 0.7em, lines.pos().join(linebreak()))

// Small text pinned to the bottom of the slide (stands in for \footnote)
#let note(body) = place(bottom + left, text(size: 0.55em, body))

#let takeaway(title, body) = block(
  width: 100%,
  fill: dark-teal,
  inset: 0.8em,
  radius: 0.2em,
  text(fill: white)[*#title* \ #body],
)

// Theorem / definition / assumption style boxes
#let thmbox(kind, name: none, body) = block(
  width: 100%,
  fill: brighter.lighten(70%),
  stroke: (left: 0.25em + bright),
  inset: 0.8em,
  radius: (right: 0.2em),
)[
  *#kind#if name != none [ (#name)]*. #body
]

// Show only the part of a PDF page that remains after trimming
// `trim: (left, bottom, right, top)` (same order as LaTeX's trim=).
#let pdf-crop(path, page: 1, trim: (0cm, 0cm, 0cm, 0cm), width: auto, height: auto) = layout(size => context {
  let (l, b, r, t) = trim
  let s = measure(image(path, page: page))
  let cw = s.width - l - r
  let ch = s.height - t - b
  let k = if width != auto {
    (if type(width) == ratio { size.width * width } else { width }) / cw
  } else if height != auto {
    height / ch
  } else { 1 }
  box(width: cw * k, height: ch * k, clip: true, place(top + left, dx: -l * k, dy: -t * k, image(
    path,
    page: page,
    width: s.width * k,
  )))
})

#let anxiety = "😰" // box(image("figures/icons/anxiety.svg", height: 1.1em), baseline: 0.2em)

// Small network drawings.
// nodes: (x, y, fill, label, label-offset); edges: pairs of node indices;
// dashed: edges (from edges) drawn dashed, with a "?", to show measurement uncertainty;
// reveal: false draws them solid with an invisible "?" so the canvas keeps the same size;
// loops: node indices that get a self-loop (loop-dir: +1 above, -1 below)
#let net(nodes, edges, dashed: (), reveal: true, loops: (), loop-dir: (:), length: 1cm, r: 0.14) = canvas(
  length: length,
  {
    import draw: *
    for (a, b) in edges {
      let (xa, ya) = (nodes.at(a).at(0), nodes.at(a).at(1))
      let (xb, yb) = (nodes.at(b).at(0), nodes.at(b).at(1))
      let unsure = (a, b) in dashed
      let st = if unsure and reveal { (paint: dark-teal, thickness: 0.08em, dash: (4pt, 3pt)) } else {
        0.08em + dark-teal
      }
      line((xa, ya), (xb, yb), stroke: st)
      if unsure {
        let len = calc.sqrt(calc.pow(xb - xa, 2) + calc.pow(yb - ya, 2))
        content(
          ((xa + xb) / 2 - (yb - ya) / len * 0.7, (ya + yb) / 2 + (xb - xa) / len * 0.7),
          text(size: 1.1em, weight: "bold", fill: if reveal { dark-teal } else { rgb(0, 0, 0, 0) })[?],
        )
      }
    }
    for i in loops {
      let d = loop-dir.at(str(i), default: 1)
      circle((nodes.at(i).at(0), nodes.at(i).at(1) + d * 0.3), radius: 0.2, stroke: 0.08em + dark-teal)
    }
    for n in nodes {
      let (x, y, fill, label, off) = n
      circle((x, y), radius: r, fill: fill, stroke: none)
      if label != none { content((x + off.at(0), y + off.at(1)), label) }
    }
  },
)

// Labelled design matrix: header row of column names above the matrix
#let design(cols, rows, delim: "[") = {
  let n = cols.len()
  math.mat(delim: delim, augment: (hline: 1, stroke: 0.05em + muted), cols, ..rows)
}

// 22.5 minutes?
#slide[
  #set page(
    header: none,
    footer: none,
    margin: 3em,
  )

  #text(size: 1.3em)[
    *Estimating peer effects in noisy, low-rank networks via network smoothing*
  ]

  #divider

  *Alex Hayes* and Keith Levin \
  November 3, 2026

  #set text(size: .8em, weight: "light")


  #place(bottom + right, image("figures/oregon-state/OSU_horizontal_2C_O_over_B.png", height: 2.3cm))
]

#slide[
  = Roadmap for today

  + What are peer effects and how do we estimate them?

  + Peer effects in latent spaces

  + Estimating peer effects from noisy data

]

#new-section[Peer effects]

#slide[
  = Humans constantly influence their social contacts

  #align(center, image("figures/friendship-diagram.png", height: 75%))

  #align(center)[
    #text(size: 0.8em)[
      Data on 241 MBA students from "Do Your Friends Stress You out? A Field Study of the Spread of Stress..." by #cite(<li2023d>, form: "prose")
    ]
  ]

]

#slide[
  = These social influences are called "peer effects" or "spillovers"

  #toolbox.side-by-side(columns: (0.35fr, 0.65fr), gutter: 1em)[
    #align(center, net(
      (
        (0, 1.25, muted, none, (0, 0)),
        (1.25, -1.25, muted, anxiety, (-1.1, -0.85)),
        (1.9, 3.1, muted, none, (0, 0)),
        (3.45, 0.6, bright, none, (0, 0)),
        (5.4, 3.0, muted, anxiety, (1.1, 0.85)),
        (4.25, -1.75, muted, anxiety, (0, -1.3)),
        (5.9, 0.1, muted, anxiety, (1.3, 0)),
      ),
      ((0, 1), (0, 2), (1, 2), (1, 3), (3, 5), (2, 3), (3, 4), (1, 5), (5, 6), (6, 4), (4, 2)),
      length: 0.8cm,
      r: 0.4,
    ))
  ][
    #set par(spacing: 1.2em)
    // #only("1-")[*Direct effect*: if I #hl[meditate], I am less likely to be anxious for my final #anxiety]

    #only("1-")[*Contagion*: if my friends are anxious #anxiety, I am more likely to be anxious #anxiety]

    // #only("1-")[*Interference*: if my friends #hl[meditate], I am less likely to be anxious #anxiety]

    #uncover("2-")[
      #text(
        size: 0.6em,
      )[\* Can be defined counterfactually, but we do not consider counterfactual inference in this talk.]
    ]
  ]
]

#slide[
  = Measuring someone's friends is challenging

  #let friends-net(reveal) = net(
    (
      (0, 1.25, muted, none, (0, 0)),
      (1.25, -1.25, muted, anxiety, (-1.1, -0.85)),
      (1.9, 3.1, muted, none, (0, 0)),
      (3.45, 0.6, bright, none, (0, 0)),
      (5.4, 3.0, muted, anxiety, (1.1, 0.85)),
      (4.25, -1.75, muted, anxiety, (0, -1.3)),
      (5.9, 0.1, muted, anxiety, (1.3, 0)),
    ),
    ((0, 1), (0, 2), (1, 2), (1, 3), (3, 5), (2, 3), (3, 4), (1, 5), (5, 6), (6, 4), (4, 2)),
    dashed: ((0, 2), (1, 3), (3, 5), (6, 4)),
    reveal: reveal,
    length: 0.8cm,
    r: 0.4,
  )

  #toolbox.side-by-side(columns: (0.35fr, 0.65fr), gutter: 1em)[
    // Both versions share one canvas size, stacked in the same spot
    #align(center, box({
      uncover("1-4", friends-net(false))
      place(top + left, uncover("5", friends-net(true)))
    }))
  ][
    #set par(spacing: 1.2em)
    #text(size: 0.8em)[
      #uncover("1-")[If I ask you who your friends are:]

      #{
        // Bullets live inside uncover so the markers appear together with their text
        let item(n, body) = uncover(str(n) + "-", grid(
          columns: (1.4em, 1fr),
          align: top + left,
          [•], body,
        ))
        set par(spacing: 0.9em)
        item(2)[answers vary depending on how exactly I ask you,]
        item(3)[you'll probably forget day-to-day social contacts, and]
        item(4)[you might think someone is your friend, but they might disagree.]
      }
    ]
  ]
]


#slide[
  = Peer effects estimators need perfectly observed networks\*

  #align(center)[
    $
      underbrace(Y_i, "outcome") = underbrace(betanaught, "intercept") + underbrace(W_i betaw, "covariate\neffects") + underbrace(betay sum_(j != i) pc(A_(i j) / d_i) Y_j, pc("average outcome\namong friends")) + underbrace(epsilon.alt_i, "noise")
    $
  ]


  #align(center, [
    #text(
      size: 0.8em,
    )[$pc(A_(i j)) in {0, 1}$ indicates a friendship between $i$ and $j$, and $d_i = sum_j A_(i j)$ is $i$'s number of friends

    ]
  ])

  #v(1em)
  #text(
    size: 0.8em,
    fill: muted,
  )[\* See #cite(<griffith2024a>, form: "prose"), #cite(<griffith2022>, form: "prose"), #cite(<lewbel2024b>, form: "prose"), #cite(<lewbel2025>, form: "prose"), #cite(<li2026b>, form: "prose") and #cite(<boucher2025>, form: "prose").]

]

#slide[
  = Hold on! Does this model even make sense?

  #text(size: 0.75em)[
    #set math.equation(numbering: none)
    #set block(spacing: 0.6em)
    Write the model in matrix form, with $G_(i j) = A_(i j) \/ d_i$ the degree-normalized adjacency matrix:

    #v(1em)

    // One aligned equation per subslide; unrevealed rows are hidden so nothing moves
    #let dgp(n) = {
      let vis(k, body) = if n >= k { body } else { hide(body) }
      let rest = $betanaught 1_n + W betaw + epsilon.alt$
      $
        #vis(1, $Y$) & #vis(1, $= betanaught 1_n + W betaw + betay G Y + epsilon.alt$) \
        #vis(2, $Y - betay G Y$) & #vis(2, $= rest$) \
        #vis(3, $Y$) & #vis(3, $= (I - betay G)^(-1) (rest)$) \
        #vis(4, $$) & #vis(4, $stretch(=)^(*) underbrace(sum_(k=0)^infinity pc(beta_"y"^k) G^k, "repeated\nneighborhood\naveraging") (rest)$)
      $
    }
    #only("1", dgp(1))
    #only("2", dgp(2))
    #only("3", dgp(3))
    #only("4-", dgp(4))

    #uncover("4-")[
      #text(size: 0.8em)[\* Must have $abs(betay) < 1$, so the effect of averaging decays with repetition]
    ]

    #v(1em)
    #uncover("5-")[
      #text(
        size: 0.8em,
      )[Because $G Y$ and $epsilon$ are correlated, cannot use ordinary least squares]
    ]
  ]
]

#slide[
  = Estimate via two-stage least squares with social instruments

  #text(size: 0.9em)[

    $
      X & = [1_n space W space G Y] wide wide wide wide wide && "usual design matrix" \
      H & = [1_n space W space G W space G^2 W]              && "peer-of-peer instruments" \
      M & = H (H^top H)^(-1) H^top                           && "projection onto instruments"
    $
    Two-stage least squares estimator
    $ hat(bbeta) = (X^top M X)^(-1) X^top M Y $

    *Theorem* @kelejian1998: Under regularity conditions
    $ sqrt(n) (hat(bbeta) - bbeta) -> cal(N)(0, Sigma_beta) $
  ]
]

#slide[
  = Peer effects estimates need perfectly observed networks

  #align(center)[
    $
      underbrace(Y_i, "outcome") = underbrace(betanaught, "intercept") + underbrace(W_i betaw, "covariate\neffects") + underbrace(betay sum_(j != i) pc(A_(i j) / d_i) Y_j, pc("average outcome\namong friends")) + underbrace(epsilon.alt_i, "noise")
    $
  ]


  #align(center, [
    #text(
      size: 0.8em,
    )[$pc(A_(i j)) in {0, 1}$ indicates a friendship between $i$ and $j$, and $d_i = sum_j A_(i j)$ is $i$'s number of friends

      ...but the edges $pc(A_(i j))$ are always measured with noise!
    ]
  ])
]

#new-section([Peer effects in latent spaces])

#slide[
  = We propose a model based on probability of friendship instead


  #text(size: 0.8em)[
    $
      Y_i & = betanaught + W_i betaw + underbrace(betay sum_(j != i) pc(A_(i j) / d_i) Y_j, pc("average outcome\namong friends")) + epsilon.alt_i & "requires all" A_(i j) \
      Y_i & = thetanaught + W_i thetaw + underbrace(thetay sum_(j != i) lc(EE[A_(i j)] / tilde(d)_i) Y_j, lc("average outcome\nweighted by\n friendship probability")) + epsilon.alt_i quad quad quad & "requires" bb(E)[A_(i j)]
    $
    #align(center, [
      #v(1em)
      #text(
        size: 0.8em,
      )[where $d_i = sum_j A_(i j)$ is degree and $tilde(d)_i = sum_j bb(E)[A_(i j)]$ is expected degree.]
    ])

    #uncover("2-")[
      Questions: (1) How to get the expected friendships $lc(bb(E)[A_(i j)])$?, and (2) Is this a good idea?
    ]
  ]
]

#slide[
  = We estimate friendships based on a low-rank network model

  #toolbox.side-by-side(gutter: 1em)[
    #align(center + horizon)[
      #image("figures/blockmodel-poster.png", height: 60%)
    ]
  ][
    *Intuition*: stochastic blockmodels

    Block indicators $Z_i$ \
    Popularity parameters $omega_i$ \
    Mixing matrix $B in [0, 1]^(r times r)$

    $ bb(P)(A_(i j) = 1 | Z, omega, B) = underbrace(omega_i Z_i B Z_j^top omega_j, "rank" r) $
  ]

  #text(
    size: 0.7em,
  )[Only assume undirected, possibly weighted $A in R^(n times n)$ with low rank $P = bb(E)[A | U, S] = U S U^top$ and sub-gamma (Bernoulli, Poisson, Gaussian, Gamma, ...) deviations of $A$ around $P$.]
]


#slide[
  = We estimate friendships based on a low-rank network model

  #toolbox.side-by-side(gutter: 1em)[
    #align(center + horizon)[
      #image("figures/blockmodel-poster.png", height: 60%)
    ]
  ][

    Let $hat(P) = hat(U) hat(S) hat(U)^top$ be rank-$r$ SVD of $A$.

    Then $hat(P)_(i j) arrow bb(E)[A_(i j)]$

    Principal components of $A$
  ]
]

#slide[
  = Is the latent contagion model a good working model?

  #text(size: 0.85em)[
    $
      Y_i & = betanaught + W_i betaw + underbrace(betay sum_(j != i) pc(A_(i j) / d_i) Y_j, pc("average outcome\namong friends")) + epsilon.alt_i & "requires all" A_(i j) \
      Y_i & = thetanaught + W_i thetaw + underbrace(thetay sum_(j != i) lc(EE[A_(i j)] / tilde(d)_i) Y_j, lc("average outcome\nweighted by\n friendship probability")) + epsilon.alt_i quad quad quad & "requires" bb(E)[A_(i j)]
    $
    #align(center, [
      #v(1em)
      #text(
        size: 0.8em,
      )[where $d_i = sum_j A_(i j)$ is degree and $tilde(d)_i = sum_j bb(E)[A_(i j)]$ is expected degree.]
    ])
  ]
]

#slide[
  = In dense networks the design matrices should be close

  The difference in the design matrices is

  $
    underbrace(sum_(j != i) pc(A_(i j) / d_i) Y_j, pc("average outcome\namong friends")) - underbrace(sum_(j != i) lc(EE[A_(i j)] / tilde(d)_i) Y_j, lc("average outcome\nweighted by\n friendship probability"))
    & = sum_(j != i) ( pc(A_(i j) / d_i) - lc(EE[A_(i j)] / tilde(d)_i)) Y_j \
    & = cal(O)_p ((sqrt(n) log(n)) / d_i)
  $
  where $A_(i j) - EE[A_(i j)]$ are i.i.d. Close when $d_i >> sqrt(n) log(n)$!
]


#slide[
  = Why are latent contagion and peer contagion models so close?

  #set figure(supplement: none)
  #set figure.caption(position: bottom)
  #toolbox.side-by-side(columns: (1fr, 1fr, 1fr), gutter: 1em)[
    #figure(image("figures/matrices/A.png", width: 100%), caption: $A$, numbering: none)
  ][
    #figure(image("figures/matrices/P.png", width: 100%), caption: $bb(E)[A] = P$, numbering: none)
  ][
    #figure(image("figures/matrices/Phat.png", width: 100%), caption: $hat(P)$, numbering: none)
  ]
]

#slide[
  = Estimate latent contagion model by plugging $hat(P)$ in for $A$

  #text(size: 0.9em)[

    Instead of using $G = D^(-1) A$,  define $hat(D) = "diag"(hat(P) 1_n)$ and $hat(G) = hat(D)^(-1) hat(P)$.

    $
        & #pc([peer contagion]) "estimator"                  &        & #lc([latent contagion]) "estimator" \
      X & = [1_n space W space G Y] wide wide wide wide wide & hat(X) & = [1_n space W space hat(G) Y] \
      H & = [1_n space W space G W space G^2 W]              & hat(H) & = [1_n space W space hat(G) W space hat(G)^2 W] \
      M & = H (H^top H)^(-1) H^top                           & hat(M) & = hat(H) (hat(H)^top hat(H))^(-1) hat(H)^top
    $
    #lc([Latent contagion]) two-stage least squares estimator
    $ thetahattsls = (hat(X)^top hat(M) hat(X))^(-1) hat(X)^top hat(M) Y $

  ]
]

#slide[
  = Latent contagion is a reasonable working model

  #text(size: 0.9em)[

    In dense networks,

    *Theorem:* $thetahattsls$ is asymptotically normal under the #lc[latent contagion] model
    $ sqrt(n) (thetahattsls - btheta) -> cal(N)(0, Sigma_theta) $

    #align(center)[and also]

    *Theorem:* $thetahattsls$ is asymptotically normal under the #pc[peer contagion] model
    $ sqrt(n) (thetahattsls - bbeta) -> cal(N)(0, Sigma_beta) $

    #text(fill: muted)[\* Omitting some details about identification for intuition]
  ]
]

#slide[
  = Asymptotically, $bbeta$ and $btheta$ have the same interpretation

  *Theorem:* In sufficiently dense low-rank networks, the population TSLS projection functionals are asymptotically equivalent:

  #text(size: 0.8em)[
    $
      & norm(
          underbrace((EE_A [X]^top M EE_A [X])^(-1) EE_A [X]^top M EE_A [Y], bbeta^* quad pc("(peer target)")) - underbrace((EE_P [tilde(X)]^top tilde(M) EE_P [tilde(X)])^(-1) EE_P [tilde(X)]^top tilde(M) EE_P [Y], btheta^* quad lc("(latent target)"))
        ) \
      & wide = o_p (1 / sqrt(n))
    $

    #v(0.5em)
    If #pc("peer contagion") is true ($bbeta^* = bbeta$), the latent target $btheta^*$ is within estimation error of $bbeta$!

    If #lc("latent contagion") is true ($btheta^* = btheta$), the peer target $bbeta^*$ is within estimation error of $btheta$!
  ]
]


#new-section[Estimating peer effects from noisy data]

#slide[
  = The latent contagion estimator doesn't need $A$, only $EE(A)$

  Recall $hat(P) = hat(U) hat(S) hat(U)^top$ estimates $EE[A]$ and $hat(D) = "diag"(hat(P) 1_n)$ and $hat(G) = hat(D)^(-1) hat(P)$.

  $
    hat(X) & = [1_n space W space hat(G) Y] \
    hat(H) & = [1_n space W space hat(G) W space hat(G)^2 W] \
    hat(M) & = hat(H) (hat(H)^top hat(H))^(-1) hat(H)^top
  $
  #lc([Latent contagion]) two-stage least squares estimator
  $ thetahattsls = (hat(X)^top hat(M) hat(X))^(-1) hat(X)^top hat(M) Y $
  #align(center, [Only needs $Y, W$ and an estimate $hat(P)$ of low-rank $P$ --  use matrix completion!])
]

#slide[
  = Estimating friendship probabilities: partial network
  #toolbox.side-by-side(gutter: 1em)[
    #align(center, [Observed data #v(0.3em) #image("figures/noisy/missing-data.png", height: 62%)])
  ][
    #align(center, [Estimated $hat(P)$ #v(0.3em) #image("figures/noisy/missing-estimate.png", height: 62%)])
  ]
  #align(center, [10% of edge missing at random (softImpute)])
]

#slide[
  = Estimating friendship probabilities: contaminated edges
  #toolbox.side-by-side(gutter: 1em)[
    #align(center, [Observed data #v(0.3em) #image("figures/noisy/gaussian-data.png", height: 62%)])
  ][
    #align(center, [Estimated $hat(P)$ #v(0.3em) #image("figures/noisy/gaussian-estimate.png", height: 62%)])
  ]

  #align(center, [Edges contaminated by mean-zero i.i.d. noise (SVD)])
]

#slide[
  = Estimating friendship probabilities: ego-centric sampling
  #toolbox.side-by-side(gutter: 1em)[
    #align(center, [Observed data #v(0.3em) #image("figures/noisy/ego-data.png", height: 62%)])
  ][
    #align(center, [Estimated $hat(P)$ #v(0.3em) #image("figures/noisy/ego-estimate.png", height: 62%)])
  ]
  #align(center, [Only see neighborhoods for 50% of nodes in network @chan2023])
]


#slide[
  = Even more exotic: _Aggregated Relational Data (ARD)_

  #toolbox.side-by-side(gutter: 5mm)[
    *Traditional network measurement*
  ][
    *Aggregated Relational Data*
  ]
  #v(2em)


  #toolbox.side-by-side(gutter: 5mm)[

    #v(1em)

    - Who are you friends with?

    - Who do you talk to often?
  ][
    How many people do you know who...

    - study statistics?

    - live in Portland?

  ]
]

#slide[
  = Estimating friendship probabilities: aggregated relational data
  #grid(
    columns: (1fr, 1fr),
    gutter: 1em,
    align(center, text(size: 24pt)[Observed data]), align(center, text(size: 24pt)[Estimated $hat(P)$]),
  )
  #toolbox.side-by-side(gutter: 1em)[

    #let draw-matrix(origin, rows, cell-w, cell-h, stroke-style, row-colors: none, col-colors: none) = {
      import draw: *
      let n-rows = rows.len()
      let n-cols = rows.at(0).len()
      let (ox, oy) = origin
      let total-w = cell-w * n-cols
      let total-h = cell-h * n-rows

      rect((ox, oy), (ox + total-w, oy + total-h), stroke: stroke-style)

      for i in range(1, n-rows) {
        line((ox, oy + i * cell-h), (ox + total-w, oy + i * cell-h), stroke: (
          paint: stroke-style.paint,
          thickness: 0.5pt,
          dash: "dotted",
        ))
      }
      for j in range(1, n-cols) {
        line((ox + j * cell-w, oy), (ox + j * cell-w, oy + total-h), stroke: (
          paint: stroke-style.paint,
          thickness: 0.5pt,
          dash: "dotted",
        ))
      }

      for (i, row) in rows.enumerate() {
        for (j, val) in row.enumerate() {
          let text-color = stroke-style.paint
          if row-colors != none {
            text-color = row-colors.at(i)
          } else if col-colors != none {
            text-color = col-colors.at(j)
          }

          let cx = ox + j * cell-w + cell-w / 2
          let cy = oy + (n-rows - 1 - i) * cell-h + cell-h / 2
          content((cx, cy), text(fill: text-color, size: 18pt, weight: "bold")[$#val$], anchor: "center")
        }
      }
    }

    #align(center + horizon)[
      #fit-width(canvas(length: 1.8cm, {
        // fit-width scales the whole drawing down to the column
        import draw: *

        let s = (paint: dark-teal, thickness: 1.8pt)
        let s-grey = (paint: muted, thickness: 1.8pt)

        let cw = 1.1
        let ch = 1.1
        let gap = 1.2

        // Data
        let a-rows = (
          ("0", "1", "1", "0", "0"),
          ("1", "0", "1", "0", "0"),
          ("1", "1", "0", "1", "0"),
          ("0", "0", "1", "0", "1"),
          ("0", "0", "0", "1", "0"),
        )
        let w-rows = (("1", "0", "0"), ("1", "0", "0"), ("0", "1", "0"), ("0", "0", "1"), ("0", "0", "1"))
        let y-rows = (("1", "1", "0"), ("1", "1", "0"), ("2", "0", "1"), ("0", "1", "1"), ("0", "0", "1"))

        // Colors
        let y-col-colors = (olive, bright, red)
        let w-row-colors = (olive, olive, bright, red, red)

        let yw = 3 * cw
        let aw = 5 * cw
        let ww = 3 * cw
        let mat-h = 5 * ch

        let y-ox = 0.0
        let a-ox = y-ox + yw + gap
        let w-ox = a-ox + aw + gap

        // ── C matrix
        draw-matrix((y-ox, 0), y-rows, cw, ch, s, col-colors: y-col-colors)
        content((y-ox + yw / 2, mat-h + 0.5), text(fill: dark-teal, size: 22pt)[C (ARD counts)], anchor: "south")
        content((y-ox + yw / 2, -0.4), text(fill: dark-teal, size: 22pt)[$NN^(n times k)$], anchor: "north")

        content((y-ox + yw + gap / 2, mat-h / 2), text(fill: dark-teal, size: 24pt)[$=$])

        // ── A matrix (muted matrix, Black labels)
        draw-matrix((a-ox, 0), a-rows, cw, ch, s-grey)
        content((a-ox + aw / 2, mat-h + 0.5), text(fill: dark-teal, size: 22pt)[A (network)], anchor: "south")
        content((a-ox + aw / 2, -0.4), text(fill: dark-teal, size: 22pt)[${0, 1}^(n times n)$], anchor: "north")

        content((a-ox + aw + gap / 2, mat-h / 2), text(fill: dark-teal, size: 24pt)[$times$])

        // ── W matrix
        draw-matrix((w-ox, 0), w-rows, cw, ch, s, row-colors: w-row-colors)
        content((w-ox + ww / 2, mat-h + 0.5), text(fill: dark-teal, size: 22pt)[W (traits)], anchor: "south")
        content((w-ox + ww / 2, -0.4), text(fill: dark-teal, size: 22pt)[${0, 1}^(n times k)$], anchor: "north")
      }))
    ]
  ][
    #align(center, [ #image("figures/noisy/ard-estimate.png", height: 62%)])
  ]
  #align(center, [Only see friend trait counts and traits (Hayes et al 2026+)])
]

#slide[
  = Simulations validate our estimator with noisy data

  // cover the right-hand "Latent TSLS" facet strip baked into the figure
  #align(center, layout(size => {
    let w = size.height * 3350 / 1884
    box(width: w, height: size.height, {
      image("figures/mse_rho_all-n34.png", width: w)
      place(top + left, dx: w * 0.95, dy: size.height * 0.2, rect(
        width: w * 0.05,
        height: size.height * 0.3,
        fill: white.darken(2%),
        stroke: none,
      ))
    })
  }))

]

#slide[
  #text(size: 0.8em)[
    #takeaway(
      "Takeaway 1",
    )[Peer effects can be estimated without observing the network exactly: use estimated friendship probabilities as the "network."]

    #v(0.5em)
    #takeaway(
      "Takeaway 2",
    )[We can use the entire toolbox of matrix completion techniques to handle network noise in low-rank networks.]


    #v(0.5em)
    #align(center, [Does plugging in $hat(EE)[A]$ in for $A$ work as a general technique to handle noise in networks?])
  ]
]

#slide[
  = Practical limitations of the latent contagion model

  #takeaway(
    "Density",
  )[Our theory requires moderately dense networks but most real world social networks are sparse]

  #takeaway(
    "Identification",
  )[It's often hard to distinguish between contagion and localized outcomes in a social network @hayes2026a @shalizi2011 ]
]


#slide[
  = Anxiety before final exams: specification matters

  #toolbox.side-by-side(columns: (0.47fr, 0.53fr), gutter: 1em)[
    #text(size: 0.7em)[
      Data on 241 MBA students from "Do Your Friends Stress You out? A Field Study of the Spread of Stress..." by #cite(<li2023d>, form: "prose")

      #v(1em)
      #align(center)[
        $
          "anxiety" & ~ "personality + demographics + U +" \
                    & quad lc("latent peer anxiety") \
          "anxiety" & ~ "personality + demographics + U +" \
                    & quad pc("peer anxiety")
        $
      ]
      #v(1em)

      $U$ accounts for localized/group effects. Dimension of $U$ is unknown!
    ]
  ][
    #align(center + horizon, image("figures/anxiety/estimates.png", width: 100%))
  ]
  #align(center, [*Model specification dramatically impacts inference*])
]

#slide[

  = Thank you! Questions? Comments?

  #toolbox.side-by-side(gutter: 5mm, columns: (1.3fr, 1fr))[

    We propose one of the first methods to estimate peer effects that works with noisy friendship measurements

    #set list(marker: none)
    #v(1em)
    #align(left)[

      #set text(size: 0.8em)

      #list(
        [#box(image("figures/icons/envelope.svg", height: 1em, fit: "contain"), baseline: 5pt) #h(0.5em) #link(
            "mailto:alex.hayes@oregonstate.edu",
          )[alex.hayes\@oregonstate.edu]],
        [#box(image("figures/icons/globe-solid-full.svg", height: 1em, fit: "contain"), baseline: 5pt) #h(0.5em) #link(
            "https://www.alexpghayes.com",
          )[alexpghayes.com]],
      )
    ]
  ][
    #align(center, image("figures/all_matrices.png", width: 100%))
  ]
]

Alex Hayes and Keith Levin, “Estimating Peer Effects in Noisy, Low-Rank   via Network Smoothing.” https://arxiv.org/abs/2605.03204

#new-section[Appendix]


#slide[
  = Assumptions of the canonical two-stage least squares estimator

  + All diagonal elements of $G$ are zero (for $hat(G)$, we set the diagonal of $hat(P)$ to zero before row-normalizing)

  + $(I - beta G)^(-1)$ is non-singular with $lr(|beta|) < 1$.

  + $G$ and $I - beta G$ are uniformly bounded in row and column sums.

  + The regressor matrices $W$ have full column rank (for $n$ large enough), and the elements of $W$ are uniformly bounded in absolute value.

  + $epsilon.alt_i$ are i.i.d. with zero mean and bounded variance $bb(E)[epsilon.alt_i^2] = sigma_(epsilon.alt)^2 < b < infinity$. Additionally $epsilon.alt_i$ have finite fourth order moments.
]


#slide[
  = Assumptions of the canonical two-stage least squares estimator

  Further, the instruments $H$ have full column rank and are composed of a subset of linearly independent columns of $mat(1_n, W, G W, G^2 W)$ where the subset contains $[1_n space W]$. Additionally
  $
    lim_(n -> oo) 1/n H^top H
  $
  is finite and non-singular and
  $
    1/n H^top X
  $
  converges in probability to a matrix that is finite and has full column rank.
]

#slide[
  = Assumptions of the canonical two-stage least squares estimator

  #text(size: 0.85em)[
    If all the previous assumptions hold, then
    $
      sqrt(n) (hat(bold(beta))_"TSLS" - bold(beta)) -> cal(N)(0, sigma_(epsilon.alt)^2 "plim"((1/n X^top M X)^(-1)))
    $
    recalling that
    $
      X = [1_n space W space G Y] quad quad quad quad G = D^(-1) A
    $
    is the covariate matrix and
    $
      M = H (H^top H)^(-1) H^top
    $
    is the projection onto the instruments $H = [1_n space W space G W space G^2 W]$.
  ]
]

#slide[
  = Estimator

  `ivreg(y ~ Gy + W | W + GW + G^2W)`, with instruments $1_n$, $W$, $G W$, $G^2 W$
]

#slide[
  = Random dot product graphs

  #text(size: 0.95em)[
    #thmbox("Definition")[
      Let $F$ be a distribution on $R^r$ such that $0 <= x^top y$ for all $x, y in "supp" F$ and the convex cone of $"supp" F$ is $r$-dimensional. Draw $Z_1, Z_2, dots, Z_n diid F$, and collect these in the rows of $Z in R^(n times r)$ for ease of notation. Conditional on these $n$ vectors, which we call _latent positions_, generate edges by drawing $\{A_(i j) : 1 <= i < j <= n\}$ as independent $(nu, b)$-subgamma random variables with $bb(E)[A_(i j) | Z] = rho Z_i^top Z_j$, where $rho in [0, 1]$. Then we say that $A$ is distributed according to an $n$-vertex random dot product graph with latent position distribution $F$, $(nu, b)$-subgamma edges and sparsity factor $rho$. We write $(A, Z) tilde "RDPG"(F, n)$.
    ]
  ]
]

#slide[
  #bibliography("latent-contagion.bib", style: "chicago-author-date")
]
