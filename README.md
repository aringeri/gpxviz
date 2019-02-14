# gpxviz
A basic tool to visualize and analyze elevation data from gpx files.
The tool takes a [gpx formatted file](https://en.wikipedia.org/wiki/GPS_Exchange_Format) as input and produces a chart (displayed on a GTK window).

![Example 1](https://github.com/aringeri/aringeri.github.io/raw/master/img/gpxviz-example1.png)

### Gpxviz makes use of the following libraries:
* HXT https://wiki.haskell.org/HXT
* Chart https://github.com/timbod7/haskell-chart/wiki

### Building:
```
cabal sandbox init
cabal install --only-dependencies
cabal build
```

### Usage
```
gpxviz <input-file.gpx>
```
