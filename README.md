# eclipse

Construction d'une image composée à partir d'une séquence photo d'éclipse solaire :
on y voit **le mouvement** du soleil dans le ciel, **l'évolution** de l'éclipse, et
**aucun disque ne se chevauche**.

Écrit en Scala 3 / SBT, à partir des fonctions de traitement d'image de
[sotohp](https://github.com/dacr/sotohp).

---

## L'idée directrice

Les photos ont été prises **sur pied, sans suivi, avec des recadrages** : la position du
soleil dans le cadre ne veut donc rien dire. Elle mélange le mouvement réel du soleil, la
dérive due à l'absence de suivi et les recadrages manuels.

D'où le principe retenu :

> **La position dans l'image ne sert qu'à découper la vignette. La position dans le
> composite vient du ciel, calculée à partir du GPS et de l'horodatage EXIF.**

Concrètement, pour chaque photo :

1. on **mesure** où est le disque solaire dans le cadre (ajustement du limbe, précis au
   sous-pixel) — cela annule dérive et recadrages ;
2. on **calcule** où était réellement le soleil (azimut, hauteur) à cet instant, depuis ce
   point GPS, avec la réfraction atmosphérique ;
3. on **place** la vignette à cette position, projetée exactement comme le ferait un
   objectif rectilinéaire.

Le composite est alors géométriquement vrai : c'est l'image qu'aurait donnée un
grand-angle unique posé pendant toute l'éclipse. La courbure de la trajectoire, sa pente,
l'espacement entre deux vues : tout est juste, et le rapport indique même la focale
équivalente en 24×36 qu'il aurait fallu.

### Le « sans chevauchement »

À 30 s d'intervalle, le soleil parcourt 0,125° alors qu'il mesure 0,53° : les disques se
recouvriraient à 80 %. La sélection est donc **géométrique, pas temporelle** : en partant
du maximum de l'éclipse, on ne garde une photo que si sa distance angulaire à la
précédente retenue dépasse la somme des rayons des deux vignettes. Le non-chevauchement
est ainsi garanti par construction, et le rapport final vérifie l'écart minimal réellement
obtenu (`smallestGapPixels`, négatif = chevauchement).

Ordres de grandeur pour ta session (200 mm, 45 MP, capteur 24×36, pas de 4,39 µm) :

| grandeur | valeur |
|---|---|
| échelle | ≈ 795 px/° |
| diamètre du soleil sur le capteur | ≈ 420 px |
| déplacement entre deux poses (30 s) | ≈ 100 px, soit 1/4 de diamètre |
| une photo retenue toutes les | ≈ 3 min (vignettes à 1,5 rayon) |
| photos retenues sur 2 h 45 de phases partielles | ≈ 50 |
| longueur de la trajectoire | ≈ 41° |

---

## Ce que fait le programme

```
analyze  →  mesure chaque photo   →  measurements.csv
plan     →  dit ce qui sera dessiné, sans rien dessiner
compose  →  dessine le composite  →  composite.png
```

La mesure (décodage RAW compris) est l'étape longue ; elle est faite **une fois** et
sauvée en CSV. Ensuite, essayer une autre mise en page ou un autre rendu est immédiat — et
le CSV se relit, s'inspecte et se corrige à la main si une photo a été mal mesurée.

Le `Makefile` sert d'aide-mémoire et suppose un lien `photos-eclipse` dans le répertoire
courant :

```bash
make            # la liste des cibles
make check      # que voit-on dans photos-eclipse, et quel décodeur RAW est installé ?
make analyze    # mesure tout, une fois pour toutes -> out/measurements.csv
make plan       # ce qui serait dessiné, sans rien dessiner
make preview    # un petit composite, pour juger
make compose    # le vrai -> out/composite.png
```

En direct :

```bash
sbt "cli/run analyze photos-eclipse --out measurements.csv"
sbt "cli/run plan measurements.csv"
sbt "cli/run compose measurements.csv --annotate --out composite.png"
```

Aucun réglage n'est nécessaire : tout est déduit des mesures (voir plus bas). Les extensions
sont insensibles à la casse : `.CR3`, `.cr3`, `.Cr3` sont traités de la même façon, comme
`.JPG` ou `.PNG`.

**Une prise de vue, pas un fichier.** Un boîtier réglé en RAW+JPEG écrit `IMG_1234.CR3`
et `IMG_1234.JPG` : c'est **une seule photo**. Les fichiers sont donc regroupés par nom
(même répertoire, même préfixe, casse indifférente) et comptés une fois. Les deux ne sont
pas interchangeables pour autant :

- les **métadonnées** sont lues dans les deux et fusionnées, le JPEG d'abord : un conteneur
  RAW récent cache souvent sa position GPS et jusqu'à sa date de prise de vue aux
  bibliothèques de lecture, alors que le JPEG écrit par le même boîtier les donne sans
  broncher. Ce qui manque à l'un est comblé par l'autre ;
- les **pixels** viennent du RAW, meilleure matière, sauf si aucun décodeur n'est installé —
  le JPEG fait alors très bien l'affaire. `--prefer-jpeg` force ce choix (bien plus rapide
  pour un premier essai).

Si aucun des deux fichiers ne donne la date, une date de création est cherchée dans
n'importe quelle section du fichier avant d'abandonner.

**Le GPS manquant n'est pas un problème.** Toutes les photos viennent du même endroit, donc
une seule position est consolidée pour toute la séance : c'est la **médiane des relevés
disponibles**, et elle sert à toutes les photos. Une seule photo géolocalisée suffit donc à
placer les 300 autres ; et comme c'est une médiane, un relevé aberrant ne déplace rien, la
gigue habituelle du récepteur est lissée, et la trajectoire calculée ne tremble plus.
L'écart maximal entre les relevés est rapporté — s'il dépasse 200 m, c'est signalé, car
l'hypothèse « même endroit » devient douteuse. `--observer` ne sert que si **aucune** photo
ne porte de position.

## Les réglages trouvés tout seuls

`plan` et `compose` commencent par mesurer, puis annoncent ce qu'ils ont décidé :

```
automatic : plate scale measured at 795 px/°, solar disc 421 px on the sensor
automatic : tile radius set to 1.35 disc radius, just enough for the edges to fade out
automatic : totality tile radius set to 2.42 disc radius, recorded corona reaches 2.20 radius
automatic : 52 frames kept out of 331, spaced so that no two discs touch
automatic : drawn at 62% of the sensor resolution, 520 px per solar disc, otherwise the
            composite would reach 32600 x 9100 px
automatic : composite will be about 20200 x 5600 px for a 41.2° x 11.4° field
```

| ce qui est réglé | comment |
|---|---|
| échelle du montage (px/°) | mesurée sur les disques eux-mêmes, rapportée au demi-diamètre de l'éphéméride |
| rayon du disque en sortie | le plus grand possible sans dépasser la résolution réelle du capteur ni la taille de sortie demandée (`--max-pixels`, `--max-side`) |
| taille des vignettes | juste ce qu'il faut pour le fondu des bords, réduit si le soleil frôle un bord de cadre (mesuré photo par photo) |
| taille des vignettes de totalité | l'étendue de couronne **réellement enregistrée**, mesurée par moyennes sur anneaux concentriques |
| choix de la mise en page | trajectoire si le soleil a bougé, planche contact sinon |
| résolution d'analyse | augmentée automatiquement si le disque est petit dans le cadre |
| rayon imposé au recalage | issu de l'échelle, ou de l'optique (focale + pas des photosites lus dans l'EXIF) avant même d'avoir regardé une image |
| point de départ de l'analyse | une vue de totalité, repérée à l'exposition |
| identification des vues sans filtre | par l'IL EXIF, comparé à la médiane de la séance |
| position de l'observateur | médiane des relevés GPS de la séance, appliquée à toutes les photos, y compris celles qui n'en ont pas |
| regroupement RAW+JPEG | par préfixe de nom ; métadonnées fusionnées, pixels pris sur le RAW quand un décodeur existe |
| dominante, luminosité, niveau de ciel | mesurés sur chaque photo |

Toute option donnée explicitement l'emporte sur la valeur trouvée ; `--no-auto` revient aux
valeurs par défaut brutes.

### Mises en page disponibles

| `--layout` | rendu |
|---|---|
| `sky-path` (défaut) | trajectoire réelle, projection gnomonique — le mouvement tel qu'il a eu lieu |
| `sky-path-even` | même courbe, mais espacement régularisé en longueur d'arc (plus « affiche ») |
| `timeline` | bande chronologique rectiligne — l'évolution seule |
| `grid` | planche contact — l'évolution seule, très lisible |

### Principaux réglages

| option | rôle |
|---|---|
| `--max-pixels`, `--max-side` | bornes de la taille de sortie, c'est le seul réglage vraiment utile |
| `--disc-radius` | force la taille du soleil dans le composite, donc l'échelle de sortie |
| `--balanced` | même nombre de vues avant et après le maximum |
| `--frames-per-side` | fixe ce nombre, plutôt que de le déduire |
| `--separation` | 1,0 = disques jointifs, 1,05 = 5 % d'air (défaut) |
| `--tile-factor` | force la marge autour du disque |
| `--totality-factor` | force la marge autour d'une vue de totalité |
| `--no-auto` | ignore les mesures et reprend les valeurs par défaut |
| `--blend` | `lighten` par défaut : sur ciel noir, les bords de vignettes deviennent invisibles |
| `--no-sky-fix`, `--no-color-fix`, `--no-brightness-fix` | désactivent les corrections automatiques |

### Le paysage en fond

Un téléobjectif ne dit rien du lieu : chaque vue est un disque sur du noir, et le composite
les place par le calcul seul. Une **photo grand angle du même ciel, prise du même endroit**,
rapporte tout ce que le 200 mm ne pouvait pas tenir — l'horizon, les arbres, la plaine, la
couleur de l'air — et il suffit de la placer une fois pour que toute la géométrie suive.

```bash
sbt "cli/run compose measurements.csv --background IMG_3794.JPG --out composite.png"
make compose-landscape BACKGROUND=IMG_3794.JPG
```

Elle se place **par son propre soleil** : l'éphéméride dit où le soleil était à cet
instant-là, la photo dit où il est tombé sur le capteur, et les deux ensemble fixent la
direction de visée. Rien d'autre n'est demandé — l'échelle vient de la focale et du pas du
capteur lus dans les EXIF, le roulis est nul pour un boîtier tenu de niveau. Le résultat est
vérifiable à l'œil : le programme annonce **à quelle ligne de la photo passe l'horizon vrai**,
et le sol doit commencer juste en dessous.

| option | rôle |
|---|---|
| `--background <fichier>` | la photo grand angle qui sert de décor |
| `--background-margin <°>` | ciel gardé autour de la séquence pour la montrer (4° par défaut) |
| `--background-brightness <0..1>` | l'assombrit, pour que les soleils restent le sujet |
| `--background-roll <°>` | roulis du boîtier, positif quand son horizon descend vers la droite |
| `--background-sun <x,y>` | où est le soleil dessus, s'il ne se trouve pas tout seul |
| `--background-scale <px/°>` | son échelle, si les EXIF ne donnent pas l'optique |
| `--background-redraw` | redessine quand même une vignette sur son soleil |

**Son soleil compte comme une vue.** La photo de paysage a été prise pendant la séance : son
soleil *est* l'un des soleils de la séquence, à son instant et à sa place. Y poser une vignette
le dessinerait deux fois, à quelques secondes d'intervalle et rendu autrement — précisément le
chevauchement que toute la sélection existe pour éviter. La règle « aucun disque ne se touche »
s'étend donc à lui : les vues qui tomberaient dessus ne sont pas dessinées, et le soleil du
paysage ferme la séquence, au même écartement que les autres.

Trois détails qui comptent :

- **le second boîtier n'a ni GPS ni bonne heure.** La position vient de la séance — l'appareil
  n'a pas bougé — et une horloge restée sur un autre fuseau est remise d'aplomb par **heures
  entières** : c'est la seule erreur qu'un fuseau sache faire, et deux faits la tranchent, le
  soleil est visible sur la photo donc il était au-dessus de l'horizon, et la photo appartient
  à la séance donc elle a été prise autour ;
- **le soleil s'y trouve par sa forme, pas par sa moyenne.** Sur un paysage, il fait quarante
  pixels sur vingt-quatre millions : on prend le plus gros groupe connexe de pixels proches du
  maximum, et son **cadre englobant**, car le barycentre d'un croissant a déjà quitté le centre
  du disque ;
- **les deux images ne sont pas d'équerre.** Passer de la géométrie d'une vue rectilinéaire à
  celle d'une autre est *exactement* une homographie 3×3 — deux projections centrales de la
  même sphère — donc neuf multiplications par pixel, sans trigonométrie et sans approximation.
  Le canevas ne grandit que jusqu'au rectangle **inscrit** dans ce que le décor couvre, sinon
  un coin noir apparaîtrait le long du bord.

Si la séquence monte plus haut que le décor, c'est dit, et ce morceau de ciel reste noir :
une vue n'est jamais jetée parce que le paysage s'arrêtait plus bas.

---

## Les points délicats, et comment ils sont traités

**Quand rien ne marche, savoir pourquoi.** « aucune photo utilisable » ne dit rien à qui
le lit. Le programme compte donc les causes séparément — sans date, sans position, sans
disque mesuré, mesure trop douteuse — les classe par fréquence, et donne des exemples :

```
6 frames read, 0 usable
  2 without a shooting date  -> the sun position cannot be computed
  6 without a measured disc  -> the sun was not found on the image
what the analysis reported :
  4x disc detection failed : nothing compact stands out of the frame (coverage 0.6231, ...)
```

**Le seuil de détection n'est pas fixe, et le sujet doit être compact.** Sur un ciel de
crépuscule, un seuil figé attrape le ciel lui-même et le centre calculé dérive de plus de
cent pixels. Le niveau est donc relevé — ou abaissé — jusqu'à ce que ce qui ressort ait une
taille **et une compacité** plausibles : un sujet rond ne traverse pas le cadre, contrairement
au ciel, au sol ou à une branche.

**Le centre du disque, pas le centre du croissant.** Un simple barycentre des pixels
éclairés dérive de plusieurs dizaines de pixels dès que l'éclipse avance. On lance donc
des rayons depuis le barycentre, on relève le dernier point éclairé de chaque rayon, puis
on ajuste un cercle **robuste** qui ne retient que l'arc extérieur (le limbe solaire) et
rejette l'arc intérieur (le limbe lunaire). L'opération est refaite une seconde fois
depuis le centre trouvé, pour que les rayons échantillonnent le limbe uniformément.
Mesuré sur images de synthèse : erreur < 1 px et RMS de 0,3 px jusqu'à 85 % d'obscuration.

**On commence par la totalité, puis on s'en éloigne.** Les photos ne sont pas mesurées dans
l'ordre du répertoire : une vue de totalité est repérée d'abord — à l'exposition seule, sans
regarder une seule image — et sert d'ancre. La séance est ensuite parcourue **vers l'avant
et vers l'arrière** à partir de là, chaque photo transmettant à la suivante la position
qu'elle vient de mesurer. À 30 s d'intervalle le soleil a très peu bougé : cette position
est un excellent point de départ, et les croissants les plus fins — ceux qui entourent la
totalité — sont mesurés avec toute la géométrie déjà connue. Un recadrage rend simplement
l'indication invalide, elle est alors ignorée. Le décodage des RAW, lui, court en avance en
tâche de fond, dans le même ordre.

Attention au sens de l'écart d'exposition, contre-intuitif : le filtre ne rend pas les
réglages extrêmes, il les rend ordinaires. Un ND1000000 ramène le soleil à du 1/125 f/8
100 ISO, tandis que la couronne, sans filtre, demande une pose bien plus longue. Les vues de
totalité sont donc **quelques IL en dessous** du reste de la séance, pas vingt au-dessus. La
plus longue série continue de ces vues donne la totalité, et son milieu donne l'ancre — pas
un anneau de diamant.

**Le rayon connu d'avance.** L'éphéméride donne le demi-diamètre apparent du soleil, et la
première passe donne l'échelle du montage (px/°) : le rayon attendu est donc connu. Sur
les croissants très fins et les vues de totalité, l'ajustement est refait **à rayon
imposé**, ce qui ne laisse chercher que le centre — beaucoup plus stable. C'est le rôle de
la seconde passe de `analyze`.

**La totalité.** Sans filtre, il n'y a plus de photosphère à ajuster. Deux indices sont
croisés : le **contraste du limbe** (un bord franc = photosphère ; une décroissance douce =
couronne) et surtout l'**exposition EXIF** — retirer un ND1000000 fait chuter l'IL d'une
vingtaine de valeurs, ce qui identifie les vues sans filtre sans aucune analyse d'image.
Le centre vient alors du barycentre lumineux de la couronne, symétrique autour de la lune,
et le rayon de l'éphéméride. Une vue d'anneau de diamant garde son ajustement géométrique,
car son limbe est franc.

**La dominante du filtre.** Un ND1000000 colore fortement l'image, et la couleur mesurée
sur le disque sert à la neutraliser, avant de retendre chaque vignette vers une même
luminosité de référence. Les vues filtrées et les vues sans filtre cohabitent ainsi dans
la même image.

**Le ciel qui n'est pas noir.** Sur tes photos de totalité le ciel est crépusculaire, avec
un dégradé et une couleur propres. Le niveau de ciel est donc mesuré sur un **anneau
autour du sujet**, canal par canal, en médiane (une branche ou une étoile qui traverse
l'anneau ne le perturbe pas), puis soustrait. Sans cela chaque vignette apparaîtrait comme
une pastille claire sur le composite.

**L'obscuration.** Elle est mesurée sur l'image (part du disque sous le seuil de
détection), pas calculée depuis une éphéméride lunaire. C'est plus simple, c'est vérifiable
et cela permet une sélection alternative « à progression régulière »
(`FrameSelector.selectByObscurationStep`), utile pour la planche contact.

---

## Architecture

```
modules/
  imaging/   fr.janalyse.sotohp.media.imaging   ← copie locale de sotohp + ajouts
  model/     fr.janalyse.eclipse.model          types du domaine
  astro/     fr.janalyse.eclipse.astro          éphémérides, réfraction, projection
  frames/    fr.janalyse.eclipse.frames         EXIF, décodage RAW, mesure par photo
  composer/  fr.janalyse.eclipse.composer       sélection, mises en page, rendu, CSV
  cli/       fr.janalyse.eclipse.cli            ligne de commande
```

### Sur la réutilisation de sotohp

`modules/imaging` **garde le package `fr.janalyse.sotohp.media.imaging` et l'API existante
de `BasicImaging` intacte**, pour que tout puisse être reversé dans sotohp sans rupture.
Les ajouts y sont marqués `ADDED` ; les nouveaux fichiers du même package sont écrits sans
rien connaître de l'éclipse, uniquement en termes d'image :

| fichier | contenu |
|---|---|
| `BasicImaging` | copie sotohp + `crop`, `cropCenteredOn` (recentrage sous-pixel), `scaleBy`, `fitWithin`, `convertTo`, `emptyLike`, et un garde-fou sur la compression à l'enregistrement |
| `Rasters` | `GrayRaster` / `RgbRaster` / `BitMask` flottants, indépendants de la profondeur (8 ou 16 bits), percentiles par histogramme |
| `CircleFitting` | ajustement algébrique (Kåsa), ajustement à rayon imposé, ajustement robuste « arc extérieur » |
| `DiscDetector` | détection d'un sujet rond (soleil, lune, planète) avec sa position sous-pixel |
| `DiscMeasures` | obscuration, niveaux et couleurs dans le disque, niveau de ciel sur un anneau |
| `ToneMapping` / `ColorBalance` | niveaux, gamma, normalisation, neutralisation de dominante, soustraction de fond |
| `Compositing` | extraction de vignette normalisée, masque radial adouci, canevas et modes de fusion |
| `RawDecoder` | décodage RAW délégué à dcraw_emu / darktable-cli / rawtherapee-cli / ImageMagick, avec cache, extensions insensibles à la casse et découverte du fichier produit (le convertisseur choisit son nom) |
| `LinearAlgebra` | résolution de systèmes, ajustement polynomial |

Toutes ces fonctions sont utilisables seules : rien n'y suppose une éclipse, ni même une
séquence.

---

**Le décodage RAW passe par le format natif du convertisseur.** `dcraw_emu` sait écrire du
TIFF, mais un TIFF que le lecteur intégré à la JVM refuse (`Data segment out of stream`) :
le décodage réussissait et le cache était pourtant illisible. On lui demande donc son
format natif, le netpbm (PPM), qui n'a rien à interpréter — un nombre magique, trois
entiers, puis les échantillons — et que ce projet lit lui-même, en conservant les seize
bits par canal. Chaque convertisseur écrit ainsi ce qu'il fait de mieux : PPM pour LibRaw,
PNG pour darktable, TIFF pour RawTherapee.

Le cache est volumineux : une image 45 Mpix en seize bits pèse **270 Mo**, soit environ
55 Go pour une séance de 200 prises. `make clean-cache` s'en débarrasse une fois le
composite obtenu. Les fichiers y sont publiés d'un seul coup, par un renommage atomique :
le décodage prend des secondes et court en avance sur la mesure, il ne faut pas qu'une
image à moitié écrite soit lue entre temps.

## Prérequis

- JDK 21, SBT
- un décodeur RAW pour les `.CR3` : `dcraw_emu` (LibRaw, le plus contrôlable), ou
  `darktable-cli`, ou `rawtherapee-cli`, ou `magick`. Le premier disponible est utilisé,
  avec balance des blancs boîtier et **sans correction automatique de luminosité** : la
  photométrie doit rester identique d'une photo à l'autre. Les fichiers décodés sont mis
  en cache (`.eclipse-cache`).

```bash
sbt test        # 84 tests
sbt cli/run     # aide en ligne

# une session d'exemple, pour essayer sans sortir les RAW
sbt "composer/Test/runMain fr.janalyse.eclipse.composer.SampleSessionGenerator /tmp/session 120"
sbt "cli/run compose /tmp/session/measurements.csv --out /tmp/session/composite.png"
```

---

## Limites connues et suites possibles

- Le canevas de sortie est en 8 bits par canal ; les vignettes étant normalisées avant
  fusion, cela suffit, mais un canevas flottant serait meilleur pour empiler plusieurs
  expositions de couronne.
- La couronne mériterait un **HDR par fusion d'expositions** pendant la totalité, si tu as
  bracketé ; l'ossature (mesure, alignement, vignettes) est déjà là.
- Les obstructions de premier plan (branches) ne sont pas détectées : sur les vues où le
  soleil en est proche, il faut réduire `--tile-factor` ou écarter la photo.
- L'orientation est supposée fixe (trépied de niveau) ; une correction de roulis par photo
  serait à ajouter si le cadrage a été repris en biais.
- Le chemin RAW/EXIF n'a pas pu être validé sur de vrais CR3 : ni décodeur RAW ni fichiers
  Canon ici. `make check` dit ce qui manque sur ta machine.
