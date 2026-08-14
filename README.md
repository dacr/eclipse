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
| `--separation` | 1,0 = disques jointifs, 1,05 = 5 % d'air (défaut) |
| `--tile-factor` | force la marge autour du disque |
| `--totality-factor` | force la marge autour d'une vue de totalité |
| `--no-auto` | ignore les mesures et reprend les valeurs par défaut |
| `--blend` | `lighten` par défaut : sur ciel noir, les bords de vignettes deviennent invisibles |
| `--no-sky-fix`, `--no-color-fix`, `--no-brightness-fix` | désactivent les corrections automatiques |

---

## Les points délicats, et comment ils sont traités

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

## Prérequis

- JDK 21, SBT
- un décodeur RAW pour les `.CR3` : `dcraw_emu` (LibRaw, le plus contrôlable), ou
  `darktable-cli`, ou `rawtherapee-cli`, ou `magick`. Le premier disponible est utilisé,
  avec balance des blancs boîtier et **sans correction automatique de luminosité** : la
  photométrie doit rester identique d'une photo à l'autre. Les fichiers décodés sont mis
  en cache (`.eclipse-cache`).

```bash
sbt test        # 58 tests
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
