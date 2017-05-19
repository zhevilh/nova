(in-package :%nova)

(defparameter *sprite-vertex-glsl* "
#version 330 core
layout (location = 0) in vec4 vertex;

out vec2 TexCoords;

uniform mat4 modelView;
uniform mat4 projection;

uniform mat4 texCoordTransform;

void main() {
  TexCoords = (texCoordTransform * vec4(vertex.zw, 0.0, 1.0)).xy;
  gl_Position = projection * modelView * vec4(vertex.xy, 0.0, 1.0);
}
")

(defparameter *sprite-fragment-glsl* "
#version 330 core
in vec2 TexCoords;
out vec4 color;

uniform sampler2D image;
uniform vec3 spriteColor;

void main()
{
  color = vec4(spriteColor, 1.0) * texture(image, TexCoords);
}
")
