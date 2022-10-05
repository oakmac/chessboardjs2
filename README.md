# chessboard.js version 2

chessboard2 library

## Development Setup

```sh
## initial setup: install node_modules/ folder
yarn install

## produce website/chessboard2.js and build the local website
npx shadow-cljs release chessboard2 && ./scripts/website.js

## run a local web server on port 3232
npm run local-dev
```

## Tests

```sh
## Unit Tests
npx shadow-cljs compile node-tests

## Cypress
npm run cypress
```

## TODO before go-live

- [ ] variadic `removeArrow`, `removeCircle`, `removePiece` functions
- [ ] add "Rings"
  - are these separate from Circles or just an added config value?
- [ ] custom Items
  - add Duck to board, add toaster SVG
- [ ] notation should be configurable
- [ ] version the position? increment by 1 every time the position changes
- [ ] review the speed shorthand times. ie: what should "slow" and "superslow" feel like?
- [ ] draggable pieces on the board
- [ ] tap-to-move should work great

## API

- everything that chessboardjs v1 has
- `getItems()` return all items
- "pulse" a piece with some simple animations
- "bounce" a piece?
- animate an arrow
- `isAnimating` boolean
- `arrows()` returns array of the arrows on the board
- `addArrow(src, dest, '#color')`
- `addArrow({src, dest, color})`
- `removeArrow(<arrowId>, <arrowId>, etc)`
- use a `data-chessboard-draggable` property to allow items to be dropped to the board

## HTML / DOM Design

- the board-container has CSS `position: relative` and known `width` and `height` values
- the board contains DOM elements (called "items"), all of which have `position: absolute`
  - squares (usually 64)
  - pieces
  - arrows
  - dots
  - X's
  - your custom element!
- chessboard keeps an internal register of the location of these elements on the board
  and will update their position in response to a change

## Notes

- maybe use the "squares as background image" technique like Wikipedia
  - would reduce the number of DOM elements
  - but less customizable for consumers
- [duck chess](https://duckchess.com/) should be implementable
