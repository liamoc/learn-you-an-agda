/*  eqn2img: converts LaTeX equation to image, part of gladtex
    Project homepage at http://gladtex.sourceforge.net
    Copyright (C) 1999-2010 Martin G. Gulbrandsen

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

/* Uncomment this for gif support */
/*#define GIF*/

#define BACKGROUND 0xFF

#define MAX(a,b) ((a)>(b) ? (a) : (b))

/* returns least integer >= a/b */
#define DIV(a,b) (((a)/(b))*(b) == (a) ? (a)/(b) : (a)/(b) + 1)

#define USAGE "\
This utility is part of\n\
gladtex version 1.2, Copyright (C) 1999-2010 Martin G. Gulbrandsen\n\
\n\
gladtex comes with ABSOLUTELY NO WARRANTY. This is free software,\n\
and you are welcome to redistribute it under certain conditions;\n\
see the file COPYING for details.\n\
\n\
Usage: eqn2img [OPTION]...\n\
\n\
-o name     output filename\n\
-v          print verbose information\n\
-f format   store images in 'format' (png by default)\n\
-s n        set oversampling factor for antialiasing to 'n' (4 by default)\n\
-r dpi      set output resolution to 'dpi' dots per inch\n\
-p string   add 'string' to LaTeX preamble (e.g. \\usepackage{...})\n\
-e env      embed equation in \\begin{env}..\\end{env} (e.g. -e 'align*')\n\
-c colour   set foreground RGB colour (000000 by default)\n\
-b colour   set background RGB colour (A0A0A0 by default, ignored for png)\n\
-t          turn transparency OFF\n\
\n\
LaTeX maths string is read from stdin. Image written to stdout unless filename\n\
specified wit -o option. Run with -f ? to list supported image formats.\n"

#include <png.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <assert.h>
#include <stdlib.h>
#ifdef GIF
#include <gif_lib.h>
#endif

#define NEW(type,size) (type *) malloc(sizeof(type)*(size))

enum img_format {png
#ifdef GIF
,gif
#endif
};

struct rgb {
  unsigned int red;
  unsigned int green;
  unsigned int blue;
};

void split_name(char *filename, char **dir, char **base, char **ext);
char *mk_basename(char *img_name);
char *mk_dirname(char *img_name);
png_structp png_initialize(FILE *fp, int *width, int *height);
png_bytepp png_read(char *filename, int *width, int *height);
png_bytepp img_modify(png_bytepp in_img, int width, int height, int supersample,
		      int *new_width, int *new_height, int *baseline);
int png_write(png_bytepp image, char *basename, int width, int height,
	      struct rgb foreground, struct rgb background, int transparency);
#ifdef GIF
int gif_write(png_bytepp image, char *basename, int width, int height,
	      struct rgb foreground, struct rgb background, int transparency);
#endif 
int to_dvi(char *base_name, char *preamble, char *env, int verbose);
int to_ps(char *base_name, int verbose);
int to_png(char *base_name, int dpi, int supersample, int verbose);

int main(int argc, char **argv)
{
  char c;
  enum img_format format = png;
  int supersample = 4;
  char *img_name = NULL;
  char *basename = NULL;
  char *dirname = NULL;
  char *extname = NULL;
  char *png_inname = NULL;
  char *env = NULL;
  png_bytepp in_image, out_image;
  int height, width;
  int new_height, new_width, baseline;
  int retval=0;
  int verbose=0;
  int transparency=1;
  int dpi=100;
  struct rgb foreground = {0,0,0};
  struct rgb background = {0xA0,0xA0,0xA0};
  char *preamble = NULL;
  int i;

  /* parse command-line options */

  while((c = getopt(argc, argv, "f:s:o:e:tvc:b:r:p:")) != EOF) {
    switch(c) {
    case 'f':
      if(!strcmp("png", optarg)) {
	format = png;
      }
#ifdef GIF
      else if(!strcmp("gif", optarg)) {
	format = gif;
      }
#endif
      else if(optarg[0]=='?') {
	printf("Supported image formats: png%s",
#ifdef GIF
	       ", gif"
#endif
	       "\n");
	return 0;
      }
      else {
	fprintf(stderr, "\nUnknown format '%s'\n", optarg);
	return 1;
      }
      break;
    case 's':
      supersample = atoi(optarg);
      if(supersample <= 0) {
	fprintf(stderr, "\nOversampling factor %s is not a positive integer\n", optarg);
	return 1;
      }
      break;
    case 'o':
      img_name = NEW(char, strlen(optarg)+1);
      assert(img_name);
      strcpy(img_name, optarg);
      break;
    case 'e':
      env = NEW(char, strlen(optarg)+1);
      assert(env);
      strcpy(env, optarg);
      break;
    case 'v':
      verbose = 1;
      break;
    case 't':
      transparency = 0;
      break;
    case 'c':
      sscanf(optarg, "%02x%02x%02x", &foreground.red, &foreground.green, &foreground.blue);
      break;
    case 'b':
      sscanf(optarg, "%02x%02x%02x", &background.red, &background.green, &background.blue);
      break;
    case 'p':
      preamble = NEW(char, strlen(optarg)+1);
      assert(preamble);
      strcpy(preamble, optarg);
      break;
    case 'r':
      dpi = atoi(optarg);
      break;
    default:
      printf(USAGE);
      return 0;
    }
  }

  if(argc != optind) {
    printf(USAGE);
    return 0;
  }

  if(!preamble) {
    preamble = NEW(char, 1);
    assert(preamble);
    preamble[0] = '\0';
  }

  if(!env) {
    env = NEW(char, 12);
    assert(env);
    strcpy(env, "displaymath");
  }

  if(img_name) {
    split_name(img_name, &dirname, &basename, &extname);
    if(extname)
      sprintf(img_name, "%s.%s", basename, extname);
    else
      img_name = basename;
  }
  else {
    basename = NEW(char, 7);
    assert(basename);
    strcpy(basename, "noname");
    dirname = NEW(char, 2);
    assert(dirname);
    strcpy(dirname, ".");
  }

  retval = chdir(dirname);
  assert(!retval);

  /* convert LaTeX equation text to image */
  if(to_dvi(basename, preamble, env, verbose) ||
     to_ps(basename, verbose) ||
     to_png(basename, dpi, supersample, verbose)) {
    return 1;
  }

  /* read input png file */
  png_inname = NEW(char, strlen(basename)+8);
  assert(png_inname);
  sprintf(png_inname, "%s.ps.png", basename);
  in_image = png_read(png_inname, &width, &height);
  unlink(png_inname);
  if(!in_image) {
    fprintf(stderr, "\nError reading '%s'\n", png_inname);
    return 1;
  }

  /* clean up */

  free(preamble);
  free(dirname);
  free(basename);
  free(env);
  free(png_inname);

  /* process image */

  out_image = img_modify(in_image, width, height, supersample, &new_width, &new_height, &baseline);

  for(i=0; i<height; i++)
    free(in_image[i]);
  free(in_image);

  /* write output file */

  switch(format) {
  case png:
    if(verbose)
      fprintf(stderr, " -> Writing png image");
    retval = png_write(out_image, img_name, new_width, new_height, foreground, background, transparency);
    break;
#ifdef GIF
  case gif:
    if(verbose)
      fprintf(stderr, " -> Writing gif image");
    retval = gif_write(out_image, img_name, new_width, new_height, foreground, background, transparency);
    break;
#endif
  }

  /* clean up */

  for(i=0; i<new_height; i++)
    free(out_image[i]);
  free(out_image);

  if(retval) {
    fprintf(stderr, "\nError writing image to '%s'\n", img_name ? img_name : "stdout");
    return 1;
  }

  if(img_name)
    printf("%i %i %i", new_width, new_height, baseline);
  else /* image being written to stdout, use stderr instead */
    fprintf(stderr, "%i %i %i", new_width, new_height, baseline);

  return 0;
}

/*
  Decomposes filename = "dir/base.ext" into its components "dir",
  "base" and "ext". If "dir" is missing (no "/" in filename), "." is
  returned. If "ext" is missing, NULL is returned, this must be taken
  care of by the caller. To clarify:
    "dir/base.ext" -> "dir" + "base" + "ext"
    "/base.ext"    -> "" + "base" + "ext"
    "base.ext"     -> "." + "base" + "ext"
    "dir/base"     -> "dir" + "base" + NULL
    "dir/base."    -> "dir" + "base + ""
  Memory for these strings is allocated by this function.
 */

void split_name(char *filename, char **dir, char **base, char **ext)
{
  char *dot, *slash;
  slash = strrchr(filename, '/');

  if(slash==NULL) {
    *dir = NEW(char,2);
    assert(*dir);
    strcpy(*dir, ".");
  }
  else {
    *dir = NEW(char, slash-filename+1);
    assert(*dir);
    strncpy(*dir, filename, slash-filename);
    (*dir)[slash-filename] = '\0';
    filename = slash+1; /* advance pointer to after slash */
  }

  dot = strrchr(filename, '.');
  if(dot==NULL) {
    *base = NEW(char, strlen(filename)+1);
    assert(*base);
    strcpy(*base, filename);
    *ext = NULL;
  }
  else {
    *base = NEW(char, dot-filename+1);
    assert(*base);
    strncpy(*base, filename, dot-filename);
    (*base)[dot-filename] = '\0';
    filename = dot+1;
    *ext = NEW(char, strlen(filename)+1);
    assert(*ext);
    strncpy(*ext, filename, strlen(filename));
    (*ext)[strlen(filename)] = '\0';
  }
}

/*
  input:
    filename
  output:
    width, height
  returns read png image, or NULL on error
*/
png_bytepp png_read(char *filename, int *width, int *height)
{
  png_byte bit_depth, color_type;
  png_bytepp image;
  png_structp png_ptr;
  png_infop info_ptr;
  int row;
  FILE *fp;

  fp = fopen(filename, "rb");
  if(!fp)
    return NULL;

  png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
  assert(png_ptr);
  
  info_ptr = png_create_info_struct(png_ptr);
  assert(info_ptr);
  
  if(setjmp(png_jmpbuf(png_ptr))) {
    png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
    fclose(fp);
    return NULL;
  }

  png_init_io(png_ptr, fp);

  png_read_info(png_ptr, info_ptr);

  /* change pixel encoding to 8 bits grayscale */
  color_type = png_get_color_type(png_ptr, info_ptr);
  bit_depth = png_get_bit_depth(png_ptr, info_ptr);

  if(bit_depth < 8)
    png_set_packing(png_ptr);
  if(bit_depth == 16)
    png_set_strip_16(png_ptr);
  if(color_type == PNG_COLOR_TYPE_GRAY && bit_depth < 8)
    png_set_expand(png_ptr);
  if(color_type != PNG_COLOR_TYPE_GRAY) {
    fprintf(stderr, "\nInput file is not grayscale.\n");
    return NULL;
  }

  png_read_update_info(png_ptr, info_ptr);

  *width = png_get_image_width(png_ptr, info_ptr);
  *height = png_get_image_height(png_ptr, info_ptr);

  image = NEW(png_bytep, *height);
  assert(image);
  for(row=0; row<*height; row++) {
    image[row] = NEW(png_byte, *width);
    assert(image[row]);
  }

  png_read_image(png_ptr, image);
  png_read_end(png_ptr, NULL);

  png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
  fclose(fp);

  return image;
}

/*
 get_pixel

 Returns the pixel color at coordinates (row, col). Negative
 coordinates and coordinates larger than width/height are
 allowed, and will return bg_color.
*/

png_byte get_pixel(png_bytepp img, int col, int row, int width, int height, png_byte bg_color)
{
  if(row<0 || col<0 || row>=height || col>=width)
    return bg_color;

  return img[row][col];
}

/* 
   img_modify does the following:
   - find 'leading dot' in in_img (i.e. some non-white area in the leftmost end:
     the perl script prepends a . (full stop) to the equation being processed)
   - copy everything to the right of the dot to out_img (returned value), while
     downscaling+antialiasing the image (simply by averaging a 'supersample' times
     'supersample' block to a single pixel)
   - return (in "baseline") the number of pixels from the bottom of the (downscaled)
     image to the baseline of the equation

   input:
     in_img, widt, height, supersample
   output:
     new_width, new_height, baseline
   returns new png image
*/

png_bytepp img_modify(png_bytepp in_img, int width, int height, int supersample,
		      int *new_width, int *new_height, int *baseline)
{
  int row, col;
  int dot_found = 0;
  int edge_found = 0;
  int row_count = 0;
  long row_sum = 0;
  int row_center;
  int col_start = 0;
  png_bytepp out_img;
  
  assert(in_img);
  
  /* find dot: vertical center and horisontal end
     (maybe it's better to find vertical end instead of center?)
     also wipe out dot */
  for(col=0; col < width; col++) {
    int empty_col = 1;
    for(row=0; row < height; row++) {
      if((in_img[row])[col] != BACKGROUND) {
	(in_img[row])[col] = BACKGROUND;
	dot_found = 1;
	empty_col = 0;
	row_count++;
	row_sum += row;
      }
    }
    if(dot_found && empty_col) {
      /* col is 1st blank column after dot */
      col_start = col;
      break;
    }
  }
  
  if(col == width) {
    fprintf(stderr, "\nImage doesn't start with a dot\n");
    exit(1);
  }

  /* the next step is a workaround: sometimes dvips produces a wrong
     boundingbox in the ps-file (I've seen this when using the \mathbb
     font), resulting in a too wide image (the height is correct but
     the box is extended to the right). to solve this we remove any
     blank area to the right */
  edge_found=0;
  for(col=width-1; col>=0 && !edge_found; col--) {
    for(row=0; row < height; row++) {
      if((in_img[row])[col] != BACKGROUND) {
	width = col+supersample; /* this is the true width (we add a small border) */
	edge_found = 1;
	break;
      }
    }
  }
  
  /* row_sum/row_count should be the centre of the "leading dot", but
     the dot itself lies above the baseline, so we add a few pixels */
  row_center = row_sum/row_count+supersample*2;
  *new_height = DIV(height, supersample);
  *new_width = DIV(width-col_start, supersample);
  *baseline = DIV(height-row_center,supersample);
  
  /* allocate memory for new image and initialize to all background */
  out_img = NEW(png_bytep, *new_height);
  assert(out_img);
  for(row=0; row < *new_height; row++) {
    out_img[row] = NEW(png_byte, *new_width);
    assert(out_img[row]);
    memset(out_img[row], BACKGROUND, *new_width);
  }

  /* I guess the following could be done more efficiently, but this doesn't
     seem to be the bottle neck anyway (disc i/o is) */
  for(row=0; row < *new_height; row++) {
    for(col=0; col < *new_width; col++) {
      int delta, epsilon;
      long pixel_sum = 0;
      for(delta=0; delta < supersample; delta++) {
	for(epsilon=0; epsilon < supersample; epsilon++) {
	  pixel_sum += get_pixel(in_img, col_start + supersample*col + epsilon,
				 supersample*row + delta,
				 width, height, BACKGROUND);
	}
      }
      out_img[row][col] = pixel_sum/(supersample*supersample);
    }
  }

  return out_img;
}

/*  write image to png file */

int png_write(png_bytepp image, char *img_name, int width, int height,
	      struct rgb foreground, struct rgb background, int transparency)
{
  FILE *fp=NULL;
  int i;
  png_byte trans[256];
  png_structp png_ptr;
  png_color pal[256];
  png_infop info_ptr;

  for(i=0; i<256; i++) {
    if(transparency) {
      pal[i].red = foreground.red;
      pal[i].green = foreground.green;
      pal[i].blue = foreground.blue;
      trans[i] = 255-i;
    }
    else {
      pal[i].red = (i*background.red + (255-i)*foreground.red)/255;
      pal[i].green = (i*background.green + (255-i)*foreground.green)/255;
      pal[i].blue = (i*background.blue + (255-i)*foreground.blue)/255;
      trans[i] = 255;
    }
  }

  if(img_name) {
    fp = fopen(img_name, "wb");
    if(!fp)
      return -1;
  }
  else
    fp = stdout;

  png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
  assert(png_ptr);

  info_ptr = png_create_info_struct(png_ptr);
  assert(info_ptr);

  /* error handling, libpng longjmps here on any error */
  if(setjmp(png_jmpbuf(png_ptr))) {
    png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
    fclose(fp);
    return -1;
  }

  png_init_io(png_ptr, fp);

  png_set_IHDR(png_ptr, info_ptr, width, height, 8, PNG_COLOR_TYPE_PALETTE,
	       PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_BASE, PNG_FILTER_TYPE_BASE);
  png_set_PLTE(png_ptr, info_ptr, pal, 256);
  png_set_tRNS(png_ptr, info_ptr, trans, 256, NULL);

  png_write_info(png_ptr, info_ptr);
  png_write_image(png_ptr, image);
  png_write_end(png_ptr, NULL);

  png_destroy_write_struct(&png_ptr, &info_ptr);

  return 0;
}


/* write image to gif file */

#ifdef GIF
int gif_write(png_bytepp image, char *img_name, int width, int height,
	      struct rgb foreground, struct rgb background, int transparency)

{
  GifFileType *fp;
  GifColorType pal[256];
  int i, row;
  ColorMapObject *color_map;
  unsigned char gc_ext[5] = { /* graphic control extension (to get transparent background) */
    0x01, /* flags: transparency color flag on */
    0x00, /* delay time */
    0x00, /* delay time cntd */
    BACKGROUND, /* transparent color index */
    0x00  /* block terminator (probably not needed?) */
  };

  if(img_name) {
    fp = EGifOpenFileName(img_name, 0);
    if(!fp)
      return -1;
  }
  else
    fp = EGifOpenFileHandle(STDOUT_FILENO);

  for(i=0; i<256; i++) {
    pal[i].Red = (i*background.red + (255-i)*foreground.red)/255;
    pal[i].Green = (i*background.green + (255-i)*foreground.green)/255;
    pal[i].Blue = (i*background.blue + (255-i)*foreground.blue)/255;
  }
  color_map = MakeMapObject(256, pal);

  /* EGifSetGifVersion("89a"); this causes segfault (but is really required for transparency, I think) */
  EGifPutScreenDesc(fp, width, height, 256, 255, color_map);
  if(transparency) {
    EGifPutExtension(fp, 0xF9, 4, gc_ext); /* this sets transparent color to background */
  }
  EGifPutImageDesc(fp, 0, 0, width, height, 0, color_map);

  for(row=0; row<height; row++) {
    if(EGifPutLine(fp, image[row], width) != GIF_OK)
      return -1;
  }

  EGifCloseFile(fp);

  return 0;
}  
#endif

int to_dvi(char *basename, char *preamble, char *env, int verbose) {
  char *tex_name;
  char *cmd;
  FILE *tex_file;
  int retval;

  tex_name = NEW(char, strlen(basename)+5);
  assert(tex_name);
  sprintf(tex_name, "%s.tex", basename);

  if(verbose)
    fprintf(stderr, "tex -> dvi");
  
  tex_file = fopen(tex_name, "w");
  fprintf(tex_file, "\\nonstopmode\n"
	         "\\documentclass[12pt]{article}\n"
	         "\\pagestyle{empty}\n"
                 "%s\n"
                 "\\begin{document}\n"
                 "\\begin{%s}\n"
                 ".",
	         preamble, env);
  /* copy equation text from stdin to tex_file */
  while(!feof(stdin)) {
    char buf[256];
    int n;
    n = fread(buf, 1, 256, stdin);
    fwrite(buf, 1, n, tex_file);
  }
  fprintf(tex_file, "\\end{%s}\n\\end{document}\n", env);
  fclose(tex_file);

  cmd = NEW(char, strlen(tex_name) + 19);
  assert(cmd);
  sprintf(cmd, "latex %s > /dev/null", tex_name);
  retval = system(cmd);
  unlink(tex_name);
  sprintf(tex_name, "%s.aux", basename);
  unlink(tex_name);
  if(retval) {
    sprintf(tex_name, "%s.dvi", basename);
    unlink(tex_name);
    fprintf(stderr, "\nError running LaTeX\n");
    return -1;
  }
  sprintf(tex_name, "%s.log", basename);
  unlink(tex_name);

  free(tex_name);
  free(cmd);
  return 0;
}

int to_ps(char *basename, int verbose) {
  char *cmd;

  if(verbose)
    fprintf(stderr, " -> ps");

  cmd = NEW(char, 2*strlen(basename) + 46);
  sprintf(cmd, "dvips -E -o %s.ps %s.dvi > /dev/null 2> /dev/null", basename, basename);
  if(system(cmd)) {
    fprintf(stderr, "\nError running dvips\n");
    return -1;
  }
  sprintf(cmd, "%s.dvi", basename);
  unlink(cmd);
  free(cmd);
  return 0;
}

int to_png(char *basename, int dpi, int supersample, int verbose) {
  char *cmd;
  int start_x, start_y, end_x, end_y;
  int xsize, ysize, xoffset, yoffset;
  int retval;
  FILE *grep, *gs;

  if(verbose)
    fprintf(stderr, " -> png");

  cmd = NEW(char, strlen(basename)+25);
  sprintf(cmd, "grep \"%%BoundingBox\" %s.ps", basename);
  grep = popen(cmd, "r");
  retval = fscanf(grep, "%%%%BoundingBox: %i %i %i %i", &start_x, &start_y, &end_x, &end_y);
  assert(retval != EOF);
  pclose(grep);
  free(cmd);

  xsize = 1 + ((end_x - start_x) * dpi)/72;
  ysize = 1 + ((end_y - start_y) * dpi)/72;
  xsize *= supersample;
  ysize *= supersample;
  xoffset = start_x;
  yoffset = start_y;

  cmd = NEW(char, strlen(basename)+200); /* I didn't count those 200.. */
  sprintf(cmd, "gs-noX11 -sDEVICE=pngmono -sOutputFile=%s.ps.png -r%i -g%ix%i -q - %s.ps",
	  basename, dpi*supersample, xsize, ysize, basename);
  gs = popen(cmd, "w");
  fprintf(gs, "%i neg %i neg translate\n", xoffset, yoffset);
  pclose(gs);
  sprintf(cmd, "%s.ps", basename);
  unlink(cmd);
  free(cmd);
  cmd = NEW(char, strlen(basename)*2+100);
  sprintf(cmd,"convert %s.ps.png -strip %s.ps.png",basename,basename );
  system(cmd);
  free(cmd);
  return 0;
}
