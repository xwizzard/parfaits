![Screenshot of the Parfaits app](site/web/media/ogres-media-collection.webp)

## Features

Parfaits is a free and open-source virtual tabletop that you can run in your browser and use to play with your friends, forked from [Ogres](https://github.com/samcf/ogres). It aims to be a very lightweight alternative to some of the more comprehensive tools available today. Its limited core feature-set is intended to help dungeon masters quickly setup encounters and adventures with only the most important necessities.

- Instantly start preparing your game; no sign-ups or ads
- Start an online collaborative session for your friends
- Prepare and manage multiple scenes at once
- Build scenes from multiple placeable board pieces, with per-image scale and grid-anchor calibration shared across board pieces and props
- Flexible grid types -- square, hex (pointy or flat-top), and isometric variants of each, with lines/dots/circles marker styles
- Modular game rules -- toggle individual systems (initiative, lighting, measurement, and more) per game type, with a dedicated builder mode for defining new ones
- Built-in initiative tracker for streamlined encounters
- Responsive design for phones and tablets
- Easy to use for other game systems
- ... and much more planned!

## Run your own server

You can run your own instance of this application by using Docker. For more information, refer to the [wiki docs](https://github.com/samcf/ogres/wiki/Docker-Usage). The following command will install and run the application.

```sh
#!/bin/sh
docker compose up -d
```
