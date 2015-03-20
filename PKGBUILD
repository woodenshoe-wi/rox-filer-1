# Maintainer: jun7 <jun7@hush.com>

pkgname=rox
pkgver=2.11.j
pkgrel=3
branch=stable
pkgdesc="A small and fast file manager which can optionally manage the desktop background and panels"
arch=('i686' 'x86_64')
license=('GPL')
url="http://rox.sourceforge.net/desktop/"
depends=('sh' 'libsm' 'gtk2')
makedepends=('librsvg' 'python2' 'libxslt' 'git')
source=("git://github.com/jun7/rox-filer.git#branch=$branch")
#md5sums=('0eebf05a67f7932367750ebf9faf215d'
md5sums=('SKIP')

prepare() {
  cd "$srcdir/rox-filer"
  # update local files
  git pull --rebase origin $branch
}

build() {
  cd "$srcdir/rox-filer"
  ./ROX-Filer/AppRun --compile LIBS="-ldl -lm"
  xsltproc -o rox.1 "$srcdir/rox-filer/ROX-Filer/src/Docs/to_man.xsl" "$srcdir/rox-filer/ROX-Filer/src/Docs/Manual.xml"
  # finally we render a png as fallback for svg unaware menu applications
  # Attention: always make sure you check the dimensions of the source-svg,
  # you can read the dimensions via inkscape's export function
#  rsvg-convert -w 48 -h 38 -f png -o "$srcdir/rox.png" "$srcdir/rox.svg"
  rsvg-convert -w 128 -h 128 -f png -o "$srcdir/rox.png" "$srcdir/rox-filer/rox.svg"
}

package() {
  cd "$srcdir/rox-filer"
  install -d "$pkgdir/usr/share/Choices/MIME-types"
  install -m755 Choices/MIME-types/* "$pkgdir/usr/share/Choices/MIME-types/"
  cp -rp ROX-Filer "$pkgdir/usr/share/"
  rm -fr "$pkgdir"/usr/share/ROX-Filer/{src,build}
 
  install -Dm755 "$srcdir/rox-filer/rox.sh" "$pkgdir/usr/bin/rox"
  install -Dm644 rox.1 "$pkgdir/usr/share/man/man1/rox.1"
  ln -sf rox.1 "$pkgdir/usr/share/man/man1/ROX-Filer.1"

  install -Dm644 "$srcdir/rox-filer/rox.desktop" "$pkgdir/usr/share/applications/rox.desktop"
  install -Dm644 "$srcdir/rox-filer/rox.svg" "$pkgdir/usr/share/pixmaps/rox.svg"
  install -Dm644 "$srcdir/rox.png" "$pkgdir/usr/share/pixmaps/rox.png"
}
