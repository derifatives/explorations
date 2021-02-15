Things to keep in mind that could easily lead to potentially hard-to-find bugs:
- Rays go through the center of a pixel.
- The default ambient color (.2, .2, .2). I assume the default specular, diffuse, and emissions colors are (0, 0, 0).
- Numerical issues can cause the intersection to be just below the surface: move slightly towards the light before casting a ray to the light.
- The assignment is not 100% clear on whether `point` or `direction` lights can be transformed; looking at the scenes to render, this isn't needed so we assume no.
- We're using double precision. We fear floating point.
- Vertices are in absolute coordinates, not transformed, so they go in a single group.
