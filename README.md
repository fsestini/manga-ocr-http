# manga-ocr-http

Query [manga-ocr](https://github.com/kha-white/manga-ocr) via POST requests.

### Prerequisites

Haskell toolchain (ghc, cabal), [manga-ocr](https://github.com/kha-white/manga-ocr).

### Build and install

    cabal build
    cabal install
    
### Usage

Just run `manga-ocr-http`. The API is accessed at `hostname:[port]/[route-name]`.
The server listens to incoming POST requests containing binary data of a JPEG image,
and responds with the OCR result.

The server port and route endpoint are user-definable, defaulting to 8080 and `ocr`
respectively. See `manga-ocr-http --help` for the full list of options.
