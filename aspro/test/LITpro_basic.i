/*
 * LITpro_basic.i --
 *
 *	Implementation of basic routines needed by simple user model functions
 *	in LITpro software.
 *
 *-----------------------------------------------------------------------------
 *
 *      Copyright (C) 2006, the LITpro Team.
 *
 *	This file is part of LITpro.
 *
 *	LITpro is free software; you can redistribute it and/or modify it
 *	under the terms of the GNU General Public License version 2 as
 *	published by the Free Software Foundation.
 *
 *	LITpro is distributed in the hope that it will be useful, but
 *	WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *	General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with LITpro; if not, write to the Free Software Foundation,
 *	Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
 *
 *-----------------------------------------------------------------------------
 *
 *	$Id: LITpro_basic.i,v 1.27 2010-04-08 14:01:46 michel Exp $
 *	$Log: LITpro_basic.i,v $
 *	Revision 1.27  2010-04-08 14:01:46  michel
 *	- Added lpb_background()
 *
 *	Revision 1.26  2010-04-01 14:33:18  jmmc
 *	function name fixed into documentation
 *
 *	Revision 1.25  2010-03-30 13:55:37  isa
 *	function lpb_nonorm_flatten_disk added
 *
 *	Revision 1.24  2010-03-30 13:35:00  isa
 *	functions lpb_nonorm_disk and lpb_nonorm_elong_disk added
 *
 *	Revision 1.23  2009-02-10 15:58:29  jmmc
 *	Remove default maxvalue (was 1) for weight parameters
 *
 *	Revision 1.22  2008-11-21 14:46:42  isa
 *	Change of the name of the parameters of the basic functions.
 *	radius --> diameter; weight --> flux_weight ;
 *	for the "elongated functions":
 *	minor_p (with p=radius or fwhm) --> minor_axis_p (with p=diameter or fwhm)
 *	axis_ratio or fwhm_ratio  --> elong_ratio (>1 for avoiding degenerescence)
 *	orientation --> major_axis_pos_angle  (position angle on the sky)
 *	Creation of the "flattened functions":
 *	flatten_f (with f=disk, gaussian, ring) function of parameters
 *	the flatten_ratio is >1, the flattening is along the minor_axis.
 *	To be able to use the previous models (as in the Goutelas Tutorial
 *	or in the SPIE-08 paper), a test has been added in LITpro_modeler.i
 *	and LITpro_basic_deprecated.i has been created.
 *
 *	Revision 1.21  2008-11-06 11:40:49  michel
 *	- lpb_circle() slightly accelerated.
 *	- all error messages reviewed (and shortened)
 *
 *	Revision 1.20  2008-06-17 05:55:47  bosc
 *	nimimum bound of a1_coeff of limb-linear removed to allow negative value
 *
 *	Revision 1.19  2008-04-01 19:56:13  bosc
 *	ring function re-written
 *
 *	Revision 1.18  2008-03-18 09:43:39  michel
 *	- typo error corrected in lpb_elong_ring()
 *	- corrected bug in percentages _lpb_test_function()
 *
 *	Revision 1.17  2008-03-17 16:08:01  michel
 *	- (forgot comments on previous commit. Redo commit with no other
 *	  modifications)
 *	- lpb_function() now handle VMIN and VMAX fields. VMIN and VMAX fields
 *	  have been filled for all the functions.
 *	- major review of the geometric functions (limb_* not modified)
 *	- lbp_ring should be more stable on singular points.
 *	- review of all the error messages.
 *	- _lpb_test_functions() added. Check functions on singular points.
 *
 *	Revision 1.16  2008-03-17 15:59:18  michel
 *	- major review of the geometric functions (limb_* not modified)
 *
 *	Revision 1.15  2008-03-13 14:32:02  bosc
 *	change into ring function and return to _LPB_DBL_MIN in circular functions
 *
 *	Revision 1.12  2008-02-22 13:09:04  jmmc
 *	Fixed missing radius_ratio of elong_ring's UNITS
 *
 *	Revision 1.11  2008-02-21 21:00:14  bosc
 *	*** empty log message ***
 *
 *	Revision 1.10  2007-11-19 16:38:17  michel
 *	- fixed underflow in lpb_disk (w * J1(2r)/r) -> (J1(2r)/r) * w
 *
 *	Revision 1.9  2007-11-19 16:17:06  cbechet
 *	Fixed two bug in the fitter, about new_active array and rescaled step. Added documentation and unified NPARAM hash member. N_PARAM is no longer used.
 *
 *	Revision 1.8  2007-03-09 12:20:16  michel
 *	- function lpb_functions() added.
 *	
 *	Revision 1.7  2006/12/07 15:37:23  michel
 *	- added variable _LPB_DEG2RAD and few comments.
 *	
 *	Revision 1.6  2006/06/11 22:53:10  bosc
 *	test on the variables added; lpb_ring corrected;
 *	elongated functions modified (ratio b/a instead of b and a)
 *	
 *	Revision 1.5  2006/06/08 16:07:43  bosc
 *	center-to-limb darkening functions added.
 *	
 *	Revision 1.4  2006/06/06 21:57:21  bosc
 *	change of the name of the functions, more homogeneous.
 *	lpb_punct,disk,circle,gaussian,ring and lpb_elong_disk,gaussian,ring tested
 *	with a plot of the result and of its fft (=object map)
 *	
 *	Revision 1.3  2006/06/02 11:31:58  eric
 *	changed prefix oi?_ into lp?_
 *	
 *	Revision 1.2  2006/06/02 09:42:27  michel
 *	- First functions (lpb_punct, lpb_unifdisk, lpb_gaussian) made.
 *	
 *	Revision 1.1.1.1  2006/05/31 08:33:15  michel
 *	Yorick Optical-interferometry General Algorithms
 *	
 *-----------------------------------------------------------------------------
 */


local _LPB_DBL_MIN; 
/* DOCUMENT _LPB_DBL_MIN
    Smallest (in magnitude) useable floating value, consistent with
    there being no leading zeros in the mantissa. Known also as
    minimum normalized positive floating-point number.
*/
_LPB_DBL_MIN= machine_constant("DBL_MIN");

local _LPB_DBL_EPS;
/* DOCUMENT _LPB_DBL_EPS
   Smallest (in magnitude) positive floating-point number that, added 
   to 1., gives something different from 1.  Loosely referred to as the 
   "floating-point precision".
*/
_LPB_DBL_EPS = machine_constant("DBL_EPSILON");


local _LPB_PI, _LPB_DEG2RAD, _LPB_MAS2RAD;
/* DOCUMENT   _LPB_PI = value of the variable PI, to avoid any corruption
    -or- _LPB_DEG2RAD = degree to radian conversion factor
    -or- _LPB_MAS2RAD = milliarcsecond to radian conversion factor
*/
_LPB_PI = 3.141592653589793238462643383279503;
_LPB_DEG2RAD = _LPB_PI/180.0;
_LPB_MAS2RAD = _LPB_PI/180.0/3600/1000;


local _LPB_FILE;
_LPB_FILE = current_include();  // Name of current file, used by lpb_functions.
 
func lpb_functions(void)
/* DOCUMENT lpb_functions
            h = lpb_functions()

    List the dictionary of the available standard modeling functions in
    LITpro library. When called as a subroutine (first form), the list of
    functions is printed on the terminal. When called as a function (second
    form), a hash table is returned, containing various informations on
    each function (list of arguments, units for each of them, help, ...).
    This second form is particularly useful to let the GUI know of this
    information.
    
    For a function to be selected in the dictionnary, it must fulfill 3 
    conditions:
    - It is defined in the file which name is stored in variable _LPB_FILE
    - Its name must begin with prefix "lpb_"
    - A comment \/* UNITS ... *\/ has be defined.

    See file of name saved in _LPB_FILE to get examples of implementation.

SEE ALSO: h_show
*/
{
  extern _LPB_FILE;
  
  // FIXME: this list should be returned by a function in LITpro world.
  reserved_args = ["ufreq", "vfreq", "wavelength", "bandwidth"];
  
  if (am_subroutine()) {
    // For collecting the SHORTDOC lines, function names and arguments.
    shortdocs = funcnames = arguments = [];
  } else {
    h = h_new();
  }
  
  f = open(_LPB_FILE);

  while (line= rdline(f)) {
    
    split= strtok(line);   // split = [<first word>, <remainder of line>]
    
    if (split(1)=="func") {
      // We found a function. Get its name.
      name= strtok(split(2), " \t(")(1);
      if (!strmatch(name, "lpb_")) continue
      // Prefix is ok. This function will be selected if UNITS comment is
      // found within the next 10 lines (without counting the lines of
      // blocks DOCUMENTS and SHORTDOC).
      doctext = units = vmin = vmax = [];
      shortdoc = "--- missing short doc ---";
      count = 10;        /* like help_worker function defined in std.i */
      
      // Collect the comments blocks.
      while ((line= rdline(f)) && count--) {
        if (!strlen(line)) break; // Empty line outside any block.
        if (strmatch(line, "/* DOCUMENT")) {
          do {
            grow, doctext, [line];
            if (strmatch(line, "*/")) break;
          } while (line= rdline(f));
          
        } else if (strmatch(line, "/* UNITS")) {
          do {
            grow, units, [line];
            if (strmatch(line, "*/")) { units = sum(units); break; }
          } while (line= rdline(f));
          
        } else if (strmatch(line, "/* VMIN")) {
          do {
            grow, vmin, [line];
            if (strmatch(line, "*/")) { vmin = sum(vmin); break; }
          } while (line= rdline(f));
          
        } else if (strmatch(line, "/* VMAX")) {
          do {
            grow, vmax, [line];
            if (strmatch(line, "*/")) { vmax = sum(vmax); break; }
          } while (line= rdline(f));
          
        } else if (strmatch(line, "/* SHORTDOC")) {
          // One single line in this case.
          shortdoc = strtrim(strpart(line,
                     strfind("/* SHORTDOC ",line)(2):strfind("*/",line)(1)));
        }
      }
      if (!is_void(units)) {
        // Thus the function is selected for being in the library.
        
        // Get the list of arguments as a vector of strings.
        args_string = strtok(sum(print(symbol_def(name))),"(,)");
        args=[];
        while ( (arg = (args_string = strtok(args_string(2),"(,)"))(1)) ) {
          // save in the list if the argument is not in the reserved list.
          if (!is_array(where(arg == reserved_args))) grow, args, arg;
        }

        if (am_subroutine()) {
          // Collect the lines.
          grow, shortdocs, shortdoc;
          grow, funcnames, name;
          grow, arguments,
                (numberof(args) == 1 ? args(1) : args(1)+sum(", "+args(2:)));
          
        } else {
          // Called as a function.
          // Open a new hash table for this function.
          key = strpart(name,5:0);     // Remove "lpb_" prefix.
          h_set, h, key, h_new();      // New entry for this function.
          hfunc = h_get(h, key);       // Reference on this entry.

          // Save help if it was found.
          if (!is_void(doctext)) help = sum(swrite(format="%s\n",doctext));
          else doctext = "";
          
          // Save and create sub-hash tables for each field to be documented,
          // UNITS, VMIN and VMAX.
          h_set, hfunc, fullname = name,
                        args = args,
                        help = doctext,
                        units = _lpb_functions_parser(units, "UNITS", args);
          if (vmin)
            h_set, hfunc, vmin = _lpb_functions_parser(vmin, "VMIN", args);
          if (vmax)
            h_set, hfunc, vmax = _lpb_functions_parser(vmax, "VMAX", args);
        }          
      }
    }
  }
  close, f;
  if (am_subroutine()) {
    funcnames += "()";
    write, format=swrite(format="    %%-%d",max(strlen(funcnames)))+"s - %s\n",
           funcnames, shortdocs;
  } else {
    return h;
  }
}



/*---------------------------------------------------------------------------*/
/*                             PUBLIC FUNCTIONS                              */
/*---------------------------------------------------------------------------*/

func lpb_punct(ufreq, vfreq, flux_weight, x, y)
/* DOCUMENT lpb_punct(ufreq, vfreq, flux_weight, x, y)
   
    Returns the Fourier transform, at spatial frequencies (UFREQ,VFREQ)
    given in 1/rad, of a punctual object (Dirac function) at coordinates
    (X,Y) given in milliarcsecond.
    FLUX_WEIGHT is the intensity coefficient. FLUX_WEIGHT=1 means total
    energy is 1.
    
    UFREQ and VFREQ must be conformable. The returned array is always
    complex and of dims dimsof(UFREQ,VFREQ).

SEE ALSO: lpb_functions
*/
/* SHORTDOC Single point (Dirac function) */
/* UNITS flux_weight, -, x, mas, y, mas */
/* VMIN  flux_weight, 0, x,   -, y,   - */
/* VMAX  flux_weight,   -, x,   -, y,   - */
{
  return flux_weight * _lpb_shift(ufreq, vfreq, x, y);
}


func lpb_background(ufreq, vfreq, flux_weight)
/* DOCUMENT lpb_background(ufreq, vfreq, flux_weight)
   
    Returns the Fourier transform, at spatial frequencies (UFREQ,VFREQ)
    given in 1/rad, of a background. FLUX_WEIGHT is the amount of total
    energy in this background. FLUX_WEIGHT=1 means total energy is 1 (i.e.
    normalized).

    WARNING: since the size of the support of the background is not
    determined, its level (or amplitude) is undefined. Actually, it will
    depend on the size of the field of view used for plotting the image.

    The visibility for this function is a Dirac function centered at 
    frequency (0,0) with amplitude given by FLUX_WEIGHT.

    UFREQ and VFREQ must be conformable. The returned array is always
    complex and of dims dimsof(UFREQ,VFREQ).

SEE ALSO: lpb_functions
*/
/* SHORTDOC Background */
/* UNITS flux_weight, - */
/* VMIN  flux_weight, 0 */
/* VMAX  flux_weight, - */
{
  vis = array(complex, dimsof(ufreq,vfreq));
  i = where(ufreq == 0. & vfreq == 0.);
  if (is_array(i)) vis(i) = flux_weight;
  return vis;
}


/*---------------------------------------------------------------------------*/
/*                            CIRCULAR FUNCTIONS                             */
/*---------------------------------------------------------------------------*/

func lpb_disk(ufreq, vfreq, flux_weight, x, y, diameter)
/* DOCUMENT lpb_disk(ufreq, vfreq, flux_weight, x, y, diameter)
   
    Returns the Fourier transform, computed at spatial frequencies
    (UFREQ,VFREQ) given in 1/rad, of a normalized uniform disk of diameter
    DIAMETER (milliarcsecond) and centered at coordinates (X,Y)
    (milliarcsecond).

    FLUX_WEIGHT is the intensity coefficient. FLUX_WEIGHT=1 means total
    energy is 1. For a fixed value of FLUX_WEIGHT, the amplitude of this
    disk function depends on the diameter. See lpb_nonorm_disk to use the
    amplitude as a parameter.

    The function returns an error if DIAMETER is negative, so it is advised
    to set a VMIN bound to zero or more on this parameter.

    UFREQ and VFREQ must be conformable. The returned array is always
    complex and with dimensions dimsof(UFREQ,VFREQ).

SEE ALSO: lpb_functions
*/
/* SHORTDOC Uniform disk with normalized total flux */
/* UNITS flux_weight, -, x, mas, y, mas, diameter, mas */
/* VMIN  flux_weight, 0, x,   -, y,   -, diameter,   0 */
/* VMAX  flux_weight, -, x,   -, y,   -, diameter,   - */
{
  extern _LPB_PI, _LPB_MAS2RAD, _LPB_DBL_MIN;
  if (diameter < 0)
    error, "diameter < 0 not allowed: vmin bound could be set to zero";
  
  // Adding _LPB_DBL_MIN is a turn around for the singularity at r=0.
  // r is numerically unchanged where r > 2.00417e-292
  r = (_LPB_MAS2RAD *_LPB_PI * 0.5 * diameter) * abs(ufreq, vfreq) +
      _LPB_DBL_MIN;

  // Multiplying by _lpb_shift at the end ensures complex type.
  return (bessj1(2.*r)/r) * flux_weight * _lpb_shift(ufreq,vfreq,x,y);
}


func lpb_nonorm_disk(ufreq, vfreq, amplitude, x, y, diameter)
/* DOCUMENT lpb_nonorm_disk(ufreq, vfreq, amplitude, x, y, diameter)
   
    Returns the Fourier transform, computed at spatial frequencies
    (UFREQ,VFREQ) given in 1/rad, of a  uniform disk of diameter
    DIAMETER (milliarcsecond), centered at coordinates (X,Y)
    (milliarcsecond) and of amplitude AMPLITUDE.

    AMPLITUDE is the amplitude, constant over the disk. In contrast
    with the function lpb_disk, the total energy here increases with
    DIAMETER.

    The function returns an error if DIAMETER is negative, so it is
    advised to set a VMIN bound to zero or more on this parameter.
    
    UFREQ and VFREQ must be conformable. The returned array is always
    complex and with dimensions dimsof(UFREQ,VFREQ).

SEE ALSO: lpb_functions
*/
/* SHORTDOC Not normalized uniform disk */
/* UNITS amplitude, -, x, mas, y, mas, diameter, mas */
/* VMIN  amplitude, -, x,   -, y,   -, diameter,   0 */
/* VMAX  amplitude, -, x,   -, y,   -, diameter,   - */
{
  return _LPB_PI * (_LPB_MAS2RAD * 0.5 * diameter)^2 * \
         lpb_disk(ufreq, vfreq, amplitude, x, y, diameter);
}


func lpb_circle(ufreq, vfreq, flux_weight, x, y, diameter)
/* DOCUMENT lpb_circle_diameter(ufreq, vfreq, flux_weight, x, y, diameter)
   
    Returns the Fourier transform, computed at spatial frequencies
    (UFREQ,VFREQ) given in 1/rad, of a normalized circle of diameter
    DIAMETER (milliarcsecond) and centered at coordinates (X,Y)
    (milliarcsecond).

    FLUX_WEIGHT is the intensity coefficient. FLUX_WEIGHT=1 means total
    energy is 1. The function returns an error if DIAMETER is negative, so
    it is advised to set a vmin bound to zero or more on this parameter.

    UFREQ and VFREQ must be conformable. The returned array is always
    complex and with dimensions dimsof(UFREQ,VFREQ).

SEE ALSO: lpb_functions
*/
/* SHORTDOC Circle */
/* UNITS flux_weight, -, x, mas, y, mas, diameter, mas */
/* VMIN  flux_weight, 0, x,   -, y,   -, diameter,   0 */
/* VMAX  flux_weight,   -, x,   -, y,   -, diameter,   - */
{
  extern _LPB_PI, _LPB_MAS2RAD;
  if (diameter < 0)
    error, "diameter < 0 not allowed: vmin bound could be set to zero";

  r = (_LPB_MAS2RAD * _LPB_PI * diameter) * abs(ufreq, vfreq);
  
  // Multiplying by _lpb_shift at the end ensures complex type.
  return bessj0(r)  * flux_weight * _lpb_shift(ufreq, vfreq, x, y);
}


func lpb_gaussian(ufreq, vfreq, flux_weight, x, y, fwhm)
/* DOCUMENT lpb_gaussian(ufreq, vfreq, flux_weight, x, y, fwhm)
   
    Returns the Fourier transform, at spatial frequencies (UFREQ,VFREQ),
    of a normalized gaussian with given FWHM centered at (X,Y) coordinates.
    
    FLUX_WEIGHT is the intensity coefficient. FLUX_WEIGHT=1 means total
    energy is 1. The function returns an error if FWHM is negative, so it
    is advised to set a vmin bound to zero or more on this parameter.

    UFREQ and VFREQ must be conformable. The returned array is always
    complex and with dimensions dimsof(UFREQ,VFREQ).

SEE ALSO: lpb_functions
*/
/* SHORTDOC Gaussian */
/* UNITS flux_weight, -, x, mas, y, mas, fwhm, mas */
/* VMIN  flux_weight, 0, x,   -, y,   -, fwhm,   0 */
/* VMAX  flux_weight,   -, x,   -, y,   -, fwhm,   - */
{
  extern _LPB_PI, _LPB_MAS2RAD;
  if (fwhm < 0) 
    error, "FWHM < 0 not allowed: vmin bound could be set to zero";
  
  f = _LPB_MAS2RAD *_LPB_PI * fwhm;
  
  // Multiplying by _lpb_shift at the end ensures complex type.
  return  exp( -(f*f)/4./log(2.) * (ufreq*ufreq + vfreq*vfreq)  ) \
          * flux_weight * _lpb_shift(ufreq, vfreq, x, y);
}


func lpb_ring(ufreq, vfreq, flux_weight, x, y, diameter, width)
/* DOCUMENT lpb_ring(ufreq, vfreq, flux_weight, x, y, diameter, width)
   
    Returns the Fourier transform, at spatial frequencies (UFREQ,VFREQ), of
    a normalized uniform ring with internal diameter DIAMETER and external
    diameter DIAMETER+WIDTH, centered at (X,Y) coordinates.
    
    FLUX_WEIGHT is the intensity coefficient. FLUX_WEIGHT=1 means total
    energy is 1. The function returns an error if DIAMETER or WIDTH is
    negative, so it is advised to set a vmin bound to zero or more on these
    parameters.

    UFREQ and VFREQ must be conformable. The returned array is always
    complex and with dimensions dimsof(UFREQ,VFREQ).

SEE ALSO: lpb_functions
*/
/* SHORTDOC Uniform ring */
/* UNITS flux_weight, -, x, mas, y, mas, diameter, mas, width, mas */
/* VMIN  flux_weight, 0, x,   -, y,   -, diameter,   0, width,   0 */
/* VMAX  flux_weight,   -, x,   -, y,   -, diameter,   -, width,   - */
{
  extern _LPB_PI, _LPB_MAS2RAD, _LPB_DBL_EPS, _LPB_DBL_MIN;
  if (diameter < 0)
    error, "diameter < 0 not allowed: vmin bound could be set to zero";
  if (width < 0)
    error, "width < 0 not allowed: vmin bound could be set to zero";

  if (width == 0.) {
    // Returns the Fourier transform of infinitely thin ring, i.e. a circle.
    return lpb_circle(ufreq, vfreq, flux_weight, x, y, diameter);
  }
  if (diameter == 0.) {
    // Returns the Fourier transform of a disk of radius width.
    return lpb_disk(ufreq, vfreq, flux_weight, x, y, 2.*width);
  }
  // This writting ensures that the result is normalized where
  // (ufreq,vfreq)=(0,0). Numerator and denominator of the expressions have
  // been simplified by radius (so we had to test diameter == 0 before).
  alpha = 1. + width/(0.5 * diameter);
  // r is numerically unchanged where r > 2.00417e-292
  r = (_LPB_MAS2RAD *_LPB_PI * 0.5 * diameter ) * abs(ufreq, vfreq) +
      _LPB_DBL_MIN;
  /*
  return (alpha * bessj1(2.*alpha*r) - bessj1(2.*r)) / ((alpha*alpha-1.)*r) \
    * flux_weight * _lpb_shift(ufreq,vfreq,x,y);
  this writing induces on MacIsa a floating point interrupt 
  */

  return ((alpha * bessj1(2.*alpha*r)/r) - (bessj1(2.*r)/ r))           \
    * (flux_weight/(alpha*alpha-1.))   * _lpb_shift(ufreq,vfreq,x,y);
}



/*---------------------------------------------------------------------------*/
/*                            ELONGATED FUNCTIONS                            */
/*---------------------------------------------------------------------------*/

func lpb_elong_disk(ufreq, vfreq, flux_weight, x, y,
                    minor_axis_diameter, elong_ratio, major_axis_pos_angle)
/* DOCUMENT lpb_elong_disk(ufreq, vfreq, flux_weight, x, y,
                           minor_axis_diameter, elong_ratio,
                           major_axis_pos_angle)

    Returns the Fourier transform, at spatial frequencies (UFREQ,VFREQ), of
    a normalized ellipse centered at coordinates (X,Y), with a ratio
    ELONG_RATIO between the major diameter and the minor one
    MINOR_AXIS_DIAMETER, turned from the positive vertical semi-axis (i.e.
    North direction) with angle MAJOR_AXIS_POS_ANGLE, in degrees, towards
    to the positive horizontal semi-axis (i.e. East direction). (the
    elongation is along the major_axis)
    
         |North          
         |               For avoiding degenerescence, the domain of variation
      ---|--->East       of MAJOR_AXIS_POS_ANGLE is 180 degrees, 
         |               for ex. from 0 to 180 degrees.
         |

    ELONG_RATIO = major_axis / minor_axis

    FLUX_WEIGHT is the intensity coefficient. FLUX_WEIGHT=1 means total
    energy is 1. The function returns an error if MINOR_AXIS_DIAMETER is
    negative or if ELONG_RATIO is smaller than 1, so it is advised to set a
    vmin bounds on these parameters.

    UFREQ and VFREQ must be conformable. The returned array is always
    complex and with dimensions dimsof(UFREQ,VFREQ).

SEE ALSO: lpb_functions
*/
/* SHORTDOC Ellipse (elongated disk) */
/* UNITS flux_weight, -, x, mas, y, mas, minor_axis_diameter, mas, elong_ratio, -,
   major_axis_pos_angle, degrees */
/* VMIN  flux_weight, 0, x,   -, y,   -, minor_axis_diameter,   0, elong_ratio, 1,
   major_axis_pos_angle, 0 */
/* VMAX  flux_weight, -, x,   -, y,   -, minor_axis_diameter,   -, elong_ratio, -,
   major_axis_pos_angle,  180 */
{
  if (minor_axis_diameter < 0)
    error, "minor_axis_diameter < 0 not allowed: vmin bound could be set to zero";
  
  if (elong_ratio < 1)
    error, "ELONG_RATIO < 1 not allowed: vmin bound could be set to 1";
  
  if (major_axis_pos_angle < 0)
    error, "MAJOR_AXIS_POS_ANGLE < 0 not allowed: vmin bound should be set to 0";

    if (major_axis_pos_angle > 180)
    error, "MAJOR_AXIS_POS_ANGLE > 180 not allowed: vmax bound should be set to 180";

  T_uv =_lpb_transform(ufreq, vfreq, double(elong_ratio), 1., major_axis_pos_angle);
  return lpb_disk(T_uv(..,1), T_uv(..,2), flux_weight, 0., 0., minor_axis_diameter)\
         * _lpb_shift(ufreq, vfreq, x, y);
}

func lpb_nonorm_elong_disk(ufreq, vfreq, amplitude, x, y,
                    minor_axis_diameter, elong_ratio, major_axis_pos_angle)
/* DOCUMENT lpb_nonorm_elong_disk(ufreq, vfreq, amplitude, x, y,
                           minor_axis_diameter, elong_ratio, major_axis_pos_angle)

    Returns the Fourier transform, at spatial frequencies
    (UFREQ,VFREQ), of an ellipse centered at coordinates (X,Y), of
    amplitude AMPLITUDE, with a ratio ELONG_RATIO between the major
    diameter and the minor one MINOR_AXIS_DIAMETER, turned from the
    positive vertical semi-axis (i.e. North direction) with angle
    MAJOR_AXIS_POS_ANGLE, in degrees, towards to the positive
    horizontal semi-axis (i.e. East direction). (the elongation is
    along the major_axis)
    
         |North          
         |               For avoiding degenerescence, the domain of variation
      ---|--->East       of MAJOR_AXIS_POS_ANGLE is 180 degrees, 
         |               for ex. from 0 to 180 degrees.
         |

    ELONG_RATIO = major_axis / minor_axis

    AMPLITUDE is the amplitude, constant over the ellipse. In contrast
    with the function lpb_elong_disk, the total energy here increases with
    MINOR_AXIS_DIAMETER and ELONG_RATIO.
    
    The function returns an error if MINOR_AXIS_DIAMETER is negative
    or if ELONG_RATIO is smaller than 1, so it is advised to set a
    vmin bounds on these parameters.

    UFREQ and VFREQ must be conformable. The returned array is always
    complex and with dimensions dimsof(UFREQ,VFREQ).

SEE ALSO: lpb_functions
*/
/* SHORTDOC Not normalized ellipse (elongated disk)*/
/* UNITS amplitude, -, x, mas, y, mas, minor_axis_diameter, mas, elong_ratio, -,
   major_axis_pos_angle, degrees */
/* VMIN  amplitude, -, x,   -, y,   -, minor_axis_diameter,   0, elong_ratio, 1,
   major_axis_pos_angle, 0 */
/* VMAX  amplitude, -, x,   -, y,   -, minor_axis_diameter,   -, elong_ratio, -,
   major_axis_pos_angle, 180 */
{
  return _LPB_PI * elong_ratio * (_LPB_MAS2RAD * 0.5 * minor_axis_diameter)^2 * \
         lpb_elong_disk(ufreq, vfreq, amplitude, x, y, \
               minor_axis_diameter, elong_ratio, major_axis_pos_angle) ;
}

func lpb_elong_gaussian(ufreq, vfreq, flux_weight, x, y,
                        minor_axis_fwhm, elong_ratio, major_axis_pos_angle)
/* DOCUMENT lpb_elong_gaussian(ufreq, vfreq, flux_weight, x, y,
                               minor_axis_fwhm, elong_ratio,
                               major_axis_pos_angle)

    Returns the Fourier transform, at spatial frequencies
    (UFREQ,VFREQ), of a normalized elongated gaussian centered at
    (X,Y) coordinates. The sizes of the function in two orthogonal
    directions are given by the narrowest FWHM (MINOR_AXIS_FWHM) and
    by the ratio ELONG_RATIO between the largest FWHM (MAJOR_AXIS_FWHM)
    and the MINOR_AXIS_FWHM, in the same way as for an ellipse
    (the elongation is along the major_axis):
    
    ELONG_RATIO = MAJOR_AXIS_FWHM / MINOR_AXIS_FWHM.
    
    MAJOR_AXIS_POS_ANGLE is measured in degrees, from the positive vertical
                         semi-axis (i.e. North direction) towards to the
         |North          positive horizontal semi-axis (i.e. East direction).  
         |               For avoiding degenerescence, the domain of variation
      ---|--->East       of MAJOR_AXIS_POS_ANGLE is 180 degrees, 
         |               for ex. from 0 to 180 degrees.
         |
    
    FLUX_WEIGHT is the intensity coefficient. FLUX_WEIGHT=1 means total
    energy is 1. The function returns an error if ELONG_RATIO is smaller
    than 1, so it is advised to set a vmin bound to 1 on this parameter.

    UFREQ and VFREQ must be conformable. The returned array is always
    complex and with dimensions dimsof(UFREQ,VFREQ).

SEE ALSO: lpb_functions, lpb_gaussian, lpb_elong_disk
*/
/* SHORTDOC Elongated Gaussian */
/* UNITS flux_weight, -, x, mas, y, mas, minor_axis_fwhm, mas, elong_ratio, -,
   major_axis_pos_angle, degrees */
/* VMIN  flux_weight, 0, x,   -, y,   -, minor_axis_fwhm,   0, elong_ratio, 1,
   major_axis_pos_angle, 0 */
/* VMAX  flux_weight, -, x,   -, y,   -, minor_axis_fwhm,   -, elong_ratio, -,
   major_axis_pos_angle, 180 */
{
  if (minor_axis_fwhm < 0)
    error, "minor_axis_fwhm < 0 not allowed: vmin bound could be set to zero";
  
  if (elong_ratio < 1)
    error, "ELONG_RATIO < 1 not allowed: vmin bound could be set to 1";

    if (major_axis_pos_angle < 0)
    error, "MAJOR_AXIS_POS_ANGLE < 0 not allowed: vmin bound could be set to 0";

    if (major_axis_pos_angle > 180)
    error, "MAJOR_AXIS_POS_ANGLE > 180 not allowed: vmax bound could be set to 180";

  T_uv = _lpb_transform(ufreq, vfreq, double(elong_ratio), 1., major_axis_pos_angle);
  return lpb_gaussian(T_uv(..,1),T_uv(..,2),flux_weight,0,0,minor_axis_fwhm) \
         * _lpb_shift(ufreq, vfreq, x, y);
}


func lpb_elong_ring(ufreq, vfreq, flux_weight, x, y,
                    minor_internal_diameter, elong_ratio, width, major_axis_pos_angle)
/* DOCUMENT lpb_elong_ring(ufreq, vfreq, flux_weight, x, y,
                           minor_internal_diameter, elong_ratio, width, major_axis_pos_angle)

    Returns the Fourier transform, at spatial frequencies (UFREQ,VFREQ), of
    a normalized uniform elongated ring centered at (X,Y) coordinates. The
    sizes of the function in two orthogonal directions are given by the
    narrowest internal radius (MINOR_INTERNAL_DIAMETER) and by the ratio
    ELONG_RATIO between the widest internal diameter and
    MINOR_INTERNAL_DIAMETER, in the same way as for an ellipse:
    
    ELONG_RATIO = MAJOR_INTERNAL_DIAMETER / MINOR_INTERNAL_DIAMETER.
    
    In the direction of MINOR_INTERNAL_DIAMETER, the external diameter is
    MINOR_INTERNAL_DIAMETER + WIDTH. In the direction of major_diameter,
    the width is magnified by the ratio ELONG_RATIO, so that the external
    diameter is the elongated MAJOR_INTERNAL_DIAMETER + ELONG_RATIO *
    WIDTH.
    
    MAJOR_AXIS_POS_ANGLE is measured in degrees, from the positive vertical
                         semi-axis (i.e. North direction) towards to the
         |North          positive horizontal semi-axis (i.e. East direction).  
         |               For avoiding degenerescence, the domain of variation
      ---|--->East       of MAJOR_AXIS_POS_ANGLE is 180 degrees, 
         |               for ex. from 0 to 180 degrees.
         |
    
    FLUX_WEIGHT is the intensity coefficient. FLUX_WEIGHT=1 means total
    energy is 1.
    
    The function returns an error if MINOR_INTERNAL_DIAMETER is negative or
    if ELONG_RATIO is smaller than 1, so it is advised to set a vmin bounds
    on these parameters.

    UFREQ and VFREQ must be conformable. The returned array is always
    complex and with dimensions dimsof(UFREQ,VFREQ).

SEE ALSO: lpb_functions, lpb_ring
*/
/* SHORTDOC Elongated ring */
/* UNITS flux_weight, -, x, mas, y, mas, minor_internal_diameter, mas, elong_ratio, -,
   width, mas, major_axis_pos_angle, degrees */
/* VMIN  flux_weight, 0, x,   -, y,   -, minor_internal_diameter,   0, elong_ratio, 1,
   width,   0, major_axis_pos_angle, 0 */
/* VMAX  flux_weight,   -, x,   -, y,   -, minor_internal_diameter,   -, elong_ratio, -,
   width,   -, major_axis_pos_angle, 180 */
{
  if (minor_internal_diameter < 0)
    error, "minor_internal_diameter < 0 not allowed: vmin bound could be set to zero";
  
  if (elong_ratio < 1)
    error, "elong_ratio < 1 not allowed: vmin bound could be set to 1";

    if (major_axis_pos_angle < 0)
    error, "MAJOR_AXIS_POS_ANGLE < 0 not allowed: vmin bound could be set to 0";

    if (major_axis_pos_angle > 180)
    error, "MAJOR_AXIS_POS_ANGLE > 180 not allowed: vmax bound could be set to 180";

  T_uv = _lpb_transform(ufreq, vfreq, double(elong_ratio), 1., major_axis_pos_angle);
  return lpb_ring(T_uv(..,1), T_uv(..,2), flux_weight, 0,0, minor_internal_diameter, width) \
         * _lpb_shift(ufreq, vfreq, x, y) ;
}

/*---------------------------------------------------------------------------*/
/*                            FLATTENED FUNCTIONS                            */
/*---------------------------------------------------------------------------*/

func lpb_flatten_disk(ufreq, vfreq, flux_weight, x, y,
                    major_axis_diameter, flatten_ratio, minor_axis_pos_angle)
/* DOCUMENT lpb_flatten_disk(ufreq, vfreq, flux_weight, x, y,
                           major_axis_diameter, flatten_ratio,
                           minor_axis_pos_angle)

    Returns the Fourier transform, at spatial frequencies (UFREQ,VFREQ), of
    a normalized ellipse centered at coordinates (X,Y), with a ratio
    FLATTEN_RATIO between the major diameter MAJOR_AXIS_DIAMETER and the
    minor one, turned from the positive vertical semi-axis (i.e. North
    direction) with angle MINOR_AXIS_POS_ANGLE, in degrees, towards to the
    positive horizontal semi-axis (i.e. East direction). (the flattening is
    along the minor_axis)
    
         |North          
         |               For avoiding degenerescence, the domain of variation
      ---|--->East       of MINOR_AXIS_POS_ANGLE is 180 degrees, 
         |               for ex. from 0 to 180 degrees.
         |
    
    FLATTEN_RATIO = major_axis / minor_axis

    FLUX_WEIGHT is the intensity coefficient. FLUX_WEIGHT=1 means total
    energy is 1. The function returns an error if MAJOR_AXIS_DIAMETER is
    negative or if FLATTEN_RATIO is smaller than 1, so it is advised to set
    a vmin bounds on these parameters.

    UFREQ and VFREQ must be conformable. The returned array is always
    complex and with dimensions dimsof(UFREQ,VFREQ).

SEE ALSO: lpb_functions
*/
/* SHORTDOC Ellipse (flattened disk) */
/* UNITS flux_weight, -, x, mas, y, mas, major_axis_diameter, mas, flatten_ratio, -,
   minor_axis_pos_angle, degrees */
/* VMIN  flux_weight, 0, x,   -, y,   -, major_axis_diameter,   0, flatten_ratio, 1,
   minor_axis_pos_angle, 0 */
/* VMAX  flux_weight, -, x,   -, y,   -, major_axis_diameter,   -, flatten_ratio, -,
   minor_axis_pos_angle, 180 */
{
  if (major_axis_diameter < 0)
    error, "major_axis_diameter < 0 not allowed: vmin bound could be set to zero";
  
  if (flatten_ratio < 1)
    error, "FLATTEN_RATIO < 1 not allowed: vmin bound could be set to 1";
  
  if (minor_axis_pos_angle < 0)
    error, "MINOR_AXIS_POS_ANGLE < 0 not allowed: vmin bound could be set to 0";

    if (minor_axis_pos_angle > 180)
    error, "MINOR_AXIS_POS_ANGLE > 180 not allowed: vmax bound could be set to 180";

  T_uv =_lpb_transform(ufreq, vfreq, 1./double(flatten_ratio), 1., minor_axis_pos_angle);
  return lpb_disk(T_uv(..,1), T_uv(..,2), flux_weight, 0., 0., major_axis_diameter)\
         * _lpb_shift(ufreq, vfreq, x, y);
}

func lpb_nonorm_flatten_disk(ufreq, vfreq, amplitude, x, y,
                    major_axis_diameter, flatten_ratio, minor_axis_pos_angle)
/* DOCUMENT lpb_nonorm_flatten_disk(ufreq, vfreq, amplitude, x, y,
                           major_axis_diameter, flatten_ratio, minor_axis_pos_angle)

    Returns the Fourier transform, at spatial frequencies
    (UFREQ,VFREQ), of an ellipse of amplitude AMPLITUDE, centered at
    coordinates (X,Y), with a ratio FLATTEN_RATIO between the major
    diameter MAJOR_AXIS_DIAMETER and the minor one, turned from the
    positive vertical semi-axis (i.e. North direction) with angle
    MINOR_AXIS_POS_ANGLE, in degrees, towards to the positive
    horizontal semi-axis (i.e. East direction).  (the flattening is
    along the minor_axis)
    
         |North          
         |               For avoiding degenerescence, the domain of variation
      ---|--->East       of MINOR_AXIS_POS_ANGLE is 180 degrees, 
         |               for ex. from 0 to 180 degrees.
         |
    
    FLATTEN_RATIO = major_axis / minor_axis

    AMPLITUDE is the amplitude, constant over the ellipse. In contrast
    with the function lpb_elong_disk, the total energy here increases
    with MAJOR_AXIS_DIAMETER and FLATTEN_RATIO.
    
    The function returns an error if MAJOR_AXIS_DIAMETER is negative
    or if FLATTEN_RATIO is smaller than 1, so it is advised to set a
    vmin bounds on these parameters.

    UFREQ and VFREQ must be conformable. The returned array is always
    complex and with dimensions dimsof(UFREQ,VFREQ).

SEE ALSO: lpb_functions
*/
/* SHORTDOC Not normalized Ellipse (flattened disk) */
/* UNITS amplitude, -, x, mas, y, mas, major_axis_diameter, mas, flatten_ratio, -,
   minor_axis_pos_angle, degrees */
/* VMIN  amplitude, -, x,   -, y,   -, major_axis_diameter,   0, flatten_ratio, 1,
   minor_axis_pos_angle, 0 */
/* VMAX  amplitude, -, x,   -, y,   -, major_axis_diameter,   -, flatten_ratio, -,
   minor_axis_pos_angle, 180 */
{
  return _LPB_PI * (_LPB_MAS2RAD * 0.5 * major_axis_diameter)^2 / flatten_ratio * \
    lpb_flatten_disk(ufreq, vfreq, amplitude, x, y, \
               major_axis_diameter, flatten_ratio, minor_axis_pos_angle) ;
}

func lpb_flatten_gaussian(ufreq, vfreq, flux_weight, x, y,
                        major_axis_fwhm, flatten_ratio, minor_axis_pos_angle)
/* DOCUMENT lpb_flatten_gaussian(ufreq, vfreq, flux_weight, x, y,
                                 major_axis_fwhm, flatten_ratio,
                                 minor_axis_pos_angle)

    Returns the Fourier transform, at spatial frequencies (UFREQ,VFREQ), of
    a normalized flattened gaussian centered at (X,Y) coordinates. The
    sizes of the function in two orthogonal directions are given by the
    largest FWHM (MAJOR_AXIS_FWHM) and by the ratio FLATTEN_RATIO between
    the MAJOR_AXIS_FWHM and MINOR_AXIS_FWHM, in the same way as for an
    ellipse (the flattening is along the minor_axis) :
    
    FLATTEN_RATIO = MAJOR_AXIS_FWHM / MINOR_AXIS_FWHM.
    
    MINOR_AXIS_POS_ANGLE is measured in degrees, from the positive vertical
                          semi-axis (i.e. North direction) towards to the
          |North          positive horizontal semi-axis (i.e. East direction).
          |               For avoiding degenerescence, the domain of variation
       ---|--->East       of MINOR_AXIS_POS_ANGLE is 180 degrees,
          |               for ex. from 0 to 180 degrees.
          |
    
    FLUX_WEIGHT is the intensity coefficient. FLUX_WEIGHT=1 means total
    energy is 1. The function returns an error if FLATTEN_RATIO is smaller
    than 1, so it is advised to set a vmin bound to 1 on this parameter.

    UFREQ and VFREQ must be conformable. The returned array is always
    complex and with dimensions dimsof(UFREQ,VFREQ).

SEE ALSO: lpb_functions, lpb_gaussian, lpb_flatten_disk
*/
/* SHORTDOC Flattened Gaussian */
/* UNITS flux_weight, -, x, mas, y, mas, major_axis_fwhm, mas, flatten_ratio, -,
   minor_axis_pos_angle, degrees */
/* VMIN  flux_weight, 0, x,   -, y,   -, major_axis_fwhm,   0, flatten_ratio, 1,
   minor_axis_pos_angle, 0 */
/* VMAX  flux_weight, -, x,   -, y,   -, major_axis_fwhm,   -, flatten_ratio, -,
   minor_axis_pos_angle, 180 */
{
  if (major_axis_fwhm < 0)
    error, "major_axis_fwhm < 0 not allowed: vmin bound could be set to zero";
  
  if (flatten_ratio < 1)
    error, "FLATTEN_RATIO < 1 not allowed: vmin bound could be set to 1";

    if (minor_axis_pos_angle < 0)
    error, "MINOR_AXIS_POS_ANGLE < 0 not allowed: vmin bound could be set to 0";

    if (minor_axis_pos_angle > 180)
    error, "MINOR_AXIS_POS_ANGLE > 180 not allowed: vmax bound could be set to 180";

  T_uv = _lpb_transform(ufreq, vfreq, 1./double(flatten_ratio), 1., minor_axis_pos_angle);
  return lpb_gaussian(T_uv(..,1),T_uv(..,2),flux_weight,0,0,major_axis_fwhm) \
         * _lpb_shift(ufreq, vfreq, x, y);
}


func lpb_flatten_ring(ufreq, vfreq, flux_weight, x, y,
                      major_internal_diameter, flatten_ratio, width,
                      minor_axis_pos_angle)
/* DOCUMENT lpb_flatten_ring(ufreq, vfreq, flux_weight, x, y,
                             major_internal_diameter, flatten_ratio, width,
                             minor_axis_pos_angle)

    Returns the Fourier transform, at spatial frequencies (UFREQ,VFREQ), of
    a normalized uniform flattenated ring centered at (X,Y) coordinates.
    The sizes of the function in two orthogonal directions are given by the
    widest internal diameter (MAJOR_INTERNAL_DIAMETER) and by the ratio
    FLATTEN_RATIO between it and the minor one, in the same way as for an
    ellipse (the flattening is along the minor axis):
    
    FLATTEN_RATIO = MAJOR_INTERNAL_DIAMETER / MINOR_INTERNAL_DIAMETER.
    
    In the direction of MAJOR_INTERNAL_DIAMETER, the external diameter
    remains MAJOR_INTERNAL_DIAMETER + WIDTH. In the direction of
    minor_diameter, the width is decreased by the ratio FLATTEN_RATIO, so
    that the external diameter is the flattened MINOR_INTERNAL_DIAMETER +
    WIDTH / FLATTEN_RATIO.
    
    MINOR_AXIS_POS_ANGLE is measured in degrees, from the positive vertical
                          semi-axis (i.e. North direction) towards to the
          |North          positive horizontal semi-axis (i.e. East direction).
          |               For avoiding degenerescence, the domain of variation
       ---|--->East       of MINOR_AXIS_POS_ANGLE is 180 degrees, 
          |               for ex. from 0 to 180 degrees.
          |

    FLUX_WEIGHT is the intensity coefficient. FLUX_WEIGHT=1 means total
    energy is 1.
    
    The function returns an error if MINOR_INTERNAL_DIAMETER is negative or
    if FLATTEN_RATIO is smaller than 1, so it is advised to set a vmin
    bounds on these parameters.

    UFREQ and VFREQ must be conformable. The returned array is always
    complex and with dimensions dimsof(UFREQ,VFREQ).

SEE ALSO: lpb_functions, lpb_ring
*/
/* SHORTDOC Flattened ring */
/* UNITS flux_weight, -, x, mas, y, mas, major_internal_diameter, mas, flatten_ratio, -,
   width, mas, minor_axis_pos_angle, degrees */
/* VMIN  flux_weight, 0, x,   -, y,   -, major_internal_diameter,   0, flatten_ratio, 1,
   width,   0, minor_axis_pos_angle, 0 */
/* VMAX  flux_weight, -, x,   -, y,   -, major_internal_diameter,   -, flatten_ratio, -,
   width,   -, minor_axis_pos_angle, 180 */
{
  if (major_internal_diameter < 0)
    error, "major_internal_diameter < 0 not allowed: vmin bound could be set to zero";
  
  if (flatten_ratio < 1)
    error, "flatten_ratio < 1 not allowed: vmin bound could be set to 1";

    if (minor_axis_pos_angle < 0)
    error, "MINOR_AXIS_POS_ANGLE < 0 not allowed: vmin bound could be set to 0";

    if (minor_axis_pos_angle > 180)
    error, "MINOR_AXIS_POS_ANGLE > 180 not allowed: vmax bound could be set to 180";

  T_uv = _lpb_transform(ufreq, vfreq, 1./double(flatten_ratio), 1., minor_axis_pos_angle);
  return lpb_ring(T_uv(..,1), T_uv(..,2), flux_weight, 0,0, major_internal_diameter, width) \
         * _lpb_shift(ufreq, vfreq, x, y) ;
}



/*---------------------------------------------------------------------------*/
/*                 Center TO LIMB DARKENING FUNCTIONS                        */
/*---------------------------------------------------------------------------*/
/* require "yeti_gsl.i"
 */

func lpb_limb_power(ufreq, vfreq, flux_weight, x, y, diameter, power)  
/* DOCUMENT lpb_limb_power(ufreq, vfreq, flux_weight, x, y, diameter, power)

    Returns the Fourier transform, at spatial frequencies (UFREQ,VFREQ), of
    a center-to-limb darkened disk of diameter DIAMETER, centered at
    coordinates (X,Y). The brightness distribution o, if expressed versus
    mu, which is the cosine of the azimuth of a surface element of the
    star, follows a law of power POWER, and is normalized for mu = 1
    (center of the star). o(mu) = mu^POWER.

    POWER equal to zero makes the function equivalent to lpb_disk.
    FLUX_WEIGHT is the intensity coefficient.
    NB: also works if DIAMETER=0 or abs(ufreq,vfreq)=0.  

SEE ALSO: lpb_functions
*/
/* SHORTDOC Limb-darkened disk with power law */
/* UNITS flux_weight, -, x, mas, y, mas, diameter, mas, power, - */
/* VMIN  flux_weight, 0, x,   -, y,   -, diameter,   0, power, 0 */
/* VMAX  flux_weight,   -, x,   -, y,   -, diameter,   -, power, 1 */
{
  extern _LPB_PI, _LPB_MAS2RAD, _LPB_DBL_EPS ;
  if (power <0 )                                                                    
      error, "power < 0 not allowed: vmin bound could be set to 0";

  if (power >1 )                                                                    
      error, "power > 1 not allowed: vmax bound could be set to 1";

  r = (_LPB_MAS2RAD *_LPB_PI * 0.5* diameter) * abs(ufreq, vfreq) + _LPB_DBL_EPS;
  
  k = power/2. +1.;
  return k * gsl_sf_gamma(k) * (gsl_sf_bessel_Jnu(k, 2. *r) /r^k) \
            * flux_weight  * _lpb_shift(ufreq, vfreq, x, y) ;
}

func lpb_limb_linear(ufreq, vfreq, flux_weight, x, y, diameter, a1_coeff)  
/* DOCUMENT lpb_limb_linear(ufreq, vfreq, flux_weight, x, y, diameter, a1_coeff)

    Returns the Fourier transform, at spatial frequencies (UFREQ,VFREQ), of
    a center-to-limb darkened disk of diameter DIAMETER, centered at
    coordinates (X,Y).  The brightness distribution o, if expressed versus
    mu, the cosine of the azimuth of a surface element of the star, follows
    a linear law of coefficient A1_COEFF ( [-1,1] ), and is normalized for mu = 1
    (center of the star).
    o(mu) = 1 -A1_COEFF(1-mu).
    A1_COEFF equal to zero makes the function equivalent to lpb_disk.
    A1_COEFF equal to one makes the function equivalent to lpb_power with power = 1
    FLUX_WEIGHT is the intensity coefficient.
    NB: also works if DIAMETER=0 or abs(ufreq,vfreq)=0.

SEE ALSO: lpb_functions
*/
/* SHORTDOC Limb-darkened disk with linear law */
/* UNITS flux_weight, -, x, mas, y, mas, diameter, mas, a1_coeff, - */
/* VMIN  flux_weight, 0, x,   -, y,   -, diameter,   0, a1_coeff, -1 */
/* VMAX  flux_weight,   -, x,   -, y,   -, diameter,   -, a1_coeff, 1 */
{
#if 0
  // FIXME: For testing purpose only. **** TO BE REMOVED ****
  extern prev_flux_weight, prev_diameter, prev_a1_coeff;
  if (is_void(prev_flux_weight)) {
    prev_flux_weight = flux_weight;
    prev_diameter = diameter;
    prev_a1_coeff = a1_coeff;
  }
  write, format="%12g/%12g  %12g/%12g  %12g/%12g\n",
         flux_weight, flux_weight-prev_flux_weight,
         diameter, diameter-prev_diameter,
         a1_coeff, a1_coeff-prev_a1_coeff;
  prev_flux_weight = flux_weight;
  prev_diameter = diameter;
  prev_a1_coeff = a1_coeff;
#endif
  
  extern _LPB_PI, _LPB_MAS2RAD, _LPB_DBL_EPS ;
  
  if (a1_coeff < -1)
      error, "a1_coeff < -1 not allowed: vmin bound could be set to -1";
   
  if (a1_coeff > 1)
      error, "a1_coeff > 1 not allowed: vmax bound could be set to 1";
  
  r = (_LPB_MAS2RAD *_LPB_PI * 0.5 * diameter) * abs(ufreq, vfreq) + _LPB_DBL_EPS;
   
  N_fact = 1.- a1_coeff/3.;
  return ((1-a1_coeff)* bessj1(2.*r)/r \
         + (a1_coeff*sqrt(_LPB_PI)/2. * gsl_sf_bessel_Jnu(1.5, 2.*r)/r^(1.5))) \
         * flux_weight /N_fact * _lpb_shift(ufreq, vfreq, x, y) ;
}

func lpb_limb_quadratic(ufreq, vfreq, flux_weight, x, y, diameter, a1_coeff, a2_coeff)  
/* DOCUMENT lpb_limb_quadratic(ufreq, vfreq, flux_weight, x, y, diameter, a1_coeff, a2_coeff)  

    Returns the Fourier transform, at spatial frequencies (UFREQ,VFREQ), of
    a center-to-limb darkened disk of diameter DIAMETER, centered at
    coordinates (X,Y).  The brightness distribution o, if expressed versus
    mu, the cosine of the azimuth of a surface element of the star, follows
    a quadratic law of coefficients A1_COEFF, A2_COEFF ([-1,1]), and is normalized
    for mu = 1 (center of the star).
    o(mu) = 1 -A1_COEFF(1-mu) - A2_COEFF(1-mu)^2.
    FLUX_WEIGHT is the intensity coefficient.
    NB: also works if DIAMETER=0 or abs(ufreq,vfreq)=0.

SEE ALSO: lpb_functions
 */
/* SHORTDOC Limb-darkened disk with quadratic law */
/* UNITS flux_weight, -, x, mas, y, mas, diameter, mas, a1_coeff, -, a2_coeff, - */
/* VMIN  flux_weight, 0, x,   -, y,   -, diameter,   0, a1_coeff, -1, a2_coeff, -1 */
/* VMAX  flux_weight,   -, x,   -, y,   -, diameter,   -, a1_coeff, 1, a2_coeff, 1 */
{
  extern _LPB_PI, _LPB_MAS2RAD, _LPB_DBL_EPS ;

  if (a1_coeff < -1)
      error, "a1_coeff < -1 not allowed: vmin bound could be set to -1";

  if (a1_coeff > 1)
      error, "a1_coeff > 1 not allowed: vmax bound could be set to 1";

  if (a2_coeff < -1)
      error, "a2_coeff < -1 not allowed: vmin bound could be set to -1";
   
  if (a2_coeff > 1)
      error, "a2_coeff > 1 not allowed: vmax bound could be set to 1";

  r = (_LPB_MAS2RAD *_LPB_PI * 0.5 * diameter) * abs(ufreq, vfreq) + _LPB_DBL_EPS;
  N_fact = 1.- a1_coeff/3.- a2_coeff/6.;
  
  return  ((1-a1_coeff-a2_coeff)* bessj1(2.*r)/r \
    + ((a1_coeff+2.*a2_coeff)*sqrt(_LPB_PI)/2. * gsl_sf_bessel_Jnu(1.5, 2.*r)/r^(1.5))\
    - a2_coeff * gsl_sf_bessel_Jn(2, 2.*r)/(r*r))\
    * flux_weight /N_fact * _lpb_shift(ufreq, vfreq, x, y) ;
}

func lpb_limb_sqrt(ufreq, vfreq, flux_weight, x, y, diameter, a1_coeff, a2_coeff)  
/* DOCUMENT lpb_limb_sqrt(ufreq, vfreq, flux_weight, x, y, diameter, a1_coeff, a2_coeff)  

    Returns the Fourier transform, at spatial frequencies (UFREQ,VFREQ), of
    a center-to-limb darkened disk of diameter DIAMETER, centered at
    coordinates (X,Y).  The brightness distribution o, if expressed versus
    mu, the cosine of the azimuth of a surface element of the star, follows
    a square root law of coefficients A1_COEFF, A2_COEFF  ([-1,1]), and is normalized
    for mu = 1 (center of the star).
    o(mu) = 1 -A1_COEFF(1-mu) - A2_COEFF(1-sqrt(mu)).
    FLUX_WEIGHT is the intensity coefficient.
    NB: also works if DIAMETER=0 or abs(ufreq,vfreq)=0.

SEE ALSO: lpb_functions
*/
/* SHORTDOC Limb-darkened disk with square root law */
/* UNITS flux_weight, -, x, mas, y, mas, diameter, mas, a1_coeff, -, a2_coeff, - */
/* VMIN  flux_weight, 0, x,   -, y,   -, diameter,   0, a1_coeff, -1, a2_coeff, -1 */
/* VMAX  flux_weight,   -, x,   -, y,   -, diameter,   -, a1_coeff, 1, a2_coeff, 1 */
{
  extern _LPB_PI, _LPB_MAS2RAD, _LPB_DBL_EPS ;
  
  if (a1_coeff < -1)
      error, "a1_coeff < -1 not allowed: vmin bound could be set to 0";

  if (a1_coeff > 1)
      error, "a1_coeff > 1 not allowed: vmax bound could be set to 1";

  if (a2_coeff < -1)
      error, "a2_coeff < -1 not allowed: vmin bound could be set to 0";
   
  if (a2_coeff > 1)
      error, "a2_coeff > 1 not allowed: vmax bound could be set to 1";

  r = (_LPB_MAS2RAD *_LPB_PI * 0.5 * diameter) * abs(ufreq, vfreq) + _LPB_DBL_EPS;
  N_fact = 1.- a1_coeff/3.- a2_coeff/5.;
  
  return  ((1-a1_coeff-a2_coeff)* bessj1(2.*r)/r \
    + (a1_coeff*sqrt(_LPB_PI)/2. * gsl_sf_bessel_Jnu(1.5, 2.*r)/r^(1.5)) \
    - a2_coeff * gsl_sf_gamma(1.25) *gsl_sf_bessel_Jnu(1.25, 2.*r)/r^(1.25))\
    * flux_weight /N_fact * _lpb_shift(ufreq, vfreq, x, y) ;
}

/*---------------------------------------------------------------------------*/
/*                            PRIVATE FUNCTIONS                              */
/*---------------------------------------------------------------------------*/

func _lpb_functions_parser(field_string, label, args)
/* DOCUMENT _lpb_functions_parser(field_string, label, args)
    
    Private function used by lpb_functions. FIELD_STRING is a string collected
    in file which name is stored in _LPB_FILE. FIELD_STRING are commented
    strings like:
      "UNITS flux_weight, -, x, mas, y, mas"
      "VMIN  flux_weight, 0, x,   -, y,   -"
    or more generally:
      "LABEL <arg>, <value>, <arg>, <value>, <arg>, <value> ...
      
    LABEL is the upercase string identifying FIELD_STRING. In the above
    examples, labels are "UNITS" and "VMIN".
    ARGS is an array of string which gives the names of the arguments whose
    values are to be looked for in FIELD_STRING.
    
    The function parse FIELD_STRING string and returns a hash table giving
    the corresponding value for each argument. In the above example we would
    get for UNITS:
    
    TOP (hash_table, 4 entries)
    |- flux_weight (string) ""
    |- x (string) "mas"
    `- y (string) "mas"

SEE ALSO: lpb_functions
*/
{

  // Ensure name exists. name should be defined in the caller (i.e
  // lpb_functions()). This ensure name is defined when testing directly
  // parsing function from the command line.
  if (is_void(name)) name="";
  
  // Get the contents of field_string as a vector of string
  // ["<arg>", "<value>", "<arg>", "<value>", ...]
  // First remove the label and separate the first token.
  field_string = strtok(strtrim(strpart(field_string,
      strfind(label, field_string)(2)+1:strfind("*/",field_string)(1))),", ");
      
  // Now break down field_string into a vector of tokens (strings).
  tokens = field_string(1);
  while ( (token = (field_string = strtok(field_string(2),", "))(1)) )
    grow, tokens, token;

  // Prepare the hash table to be returned
  h = h_new();
  
  // Save in the hash table the value corresponding to each argument,
  // after checking various errors.
  for (i=1; i<=numberof(args) ; ++i) {
    arg = args(i);
    index = where(tokens == arg);   // find this arg in the list of tokens.
    if (!is_array(index)) {
      // This is a programmer bug.
      error, "argument \""+arg+"\" is missing in "+label+ " of "+
             name+"()";
    }
    if (numberof(index) > 1) {
      // This is a programmer bug:
      // maybe 2 different values for the same arguments.
      error, "argument \""+arg+"\" is defined several times in "+
             label+" of "+name+"()";
    }
    value = tokens(index(1)+1);     // get the corresponding value.
    if ( value == "-" ) value = ""; // No value for this arg.
    h_set, h, arg, value;           // Save in hash table.
  }
  return h;
}


func _lpb_shift(ufreq, vfreq, x, y)
/* DOCUMENT _lpb_shift(ufreq, vfreq, x, y)
   
    Returns complex factor to apply in the Fourier transform at frequencies
    (UFREQ,VFREQ) to account for a shift (X,Y) in image space.
    X, Y are given in milliarcseconds.
SEE ALSO:
*/
{
  extern _LPB_MAS2RAD,_LPB_PI;
  phase = 2. * _LPB_PI * _LPB_MAS2RAD * (x*ufreq + y*vfreq);
  return cos(phase) - 1i*sin(phase);
}


func _lpb_transform(ufreq, vfreq, t_ana, t_homo, rotation)
/* DOCUMENT _lpb_transform(ufreq, vfreq, t_ana, t_homo, rotation)
   
    Returns the new spatial frequencies when the object has got geometrical
    transformations, successively a rotation,  an anamorphose and a homothetie.
    (u,v)--> Transpose(Inverse(T))(u,v), with matrix T = HAR;
    Inverse(R)= |cos(beta) -sin(beta)|
                |sin(beta)  cos(beta)|   beta angle in degrees
      beta is the trigonometric angle
          |y
          |
       ---|---> x    beta =0 or 180 for y=0, beta = 90 or -90 for x=0)  
          |
          | 
      
    Inverse(A)= |t_ana 0|
                |0     1|  t_ana = ratio of anamorphose, >0
    Inverse(H)= |t_homo   0|
                |0   t_homo|  t_homo = ratio of homothetie >0

    The angle ROTATION is the astronomical position angle,       |North
    equal to 0 or 180 for x=0, and  90 or 270 for y=0.           |
    so, ROTATION = 90 - beta                                  ---|--->East
    the positive x-semi-axis being the Est direction, and        |
    the positive y-semi-axis beeing the North direction.         |
    
SEE ALSO:
*/
{
  extern _LPB_DEG2RAD;
  rotation = 90. - rotation;  
  rotation *= _LPB_DEG2RAD;
  
  _ufreq =  ufreq * (cos(rotation)* t_ana* t_homo) + \
            vfreq * (sin(rotation)* t_ana* t_homo);
  _vfreq = -ufreq * (sin(rotation)* t_homo) + vfreq * (cos(rotation)* t_homo);
  return [_ufreq,_vfreq];
 
}


func _lpb_test_functions(void)
{
  extern _LPB_DBL_EPS, _LPB_DBL_MIN
  // Test if the current machine complies with IEEE standard for floating
  // values.
  if (_LPB_DBL_MIN/2 != 0) {
    IEEE_compliant = 1;
    write, format="%s\n",
           "This machine complies with IEEE standard for floating values.";
    
  } else {
    IEEE_compliant = 0;
    lp_warn,
      "This machine does not comply with IEEE standard for floating values.",
      "If a floating value is less than xmin, many computers underflow its",
      "value to zero. The IEEE standard specifies a more graceful kind of",
      "underflow: As a value becomes smaller than xmin, its exponent is",
      "frozen at the smallest allowed value, while its mantissa is decreased,",
      "acquiring leading zeros and \"gracefully\" losing precision.\n";
  }
  write, format="_LPB_DBL_MIN is: %g\n", _LPB_DBL_MIN;
  write, format="_LPB_DBL_EPS is: %g\n", _LPB_DBL_EPS;
  
  // Bessel functions
  _lpb_test_function, "bessj1",
    //       answer,              x
    [[ _LPB_DBL_MIN, 2*_LPB_DBL_MIN],
     [ _LPB_DBL_EPS, 2*_LPB_DBL_EPS]];

  // lpb_disk
  _lpb_test_function, "lpb_disk",
    // answer, ufreq, vfreq, flux_weight,   x,   y, diameter
    [[     1.,    0.,    0.,     1.,  0.,  0.,     1.],
     [     1.,    0.,    0.,     1.,  0.,  0.,     0.],
     [     1.,   1e9,   1e9,     1.,  0.,  0.,     0.]],
     "         ufreq, vfreq, flux_weight, x, y, diameter";

  // lpb_circle
  _lpb_test_function, "lpb_circle",
    [[     1.,    0.,    0.,     1.,  0.,  0.,     1.],
     [     1.,    0.,    0.,     1.,  0.,  0.,     0.],
     [     1.,   1e9,   1e9,     1.,  0.,  0.,     0.]],
     "         ufreq, vfreq, flux_weight, x, y, diameter";

  // lpb_gaussian
  _lpb_test_function, "lpb_gaussian",
    // anwser, ufreq, vfreq, flux_weight,   x,   y,  fwhm
    [[     1.,    0.,    0.,     1.,  0.,  0.,     0.],
     [     1.,   1e9,   1e9,     1.,  0.,  0.,     0.]],
     "         ufreq, vfreq, flux_weight, x, y, fwhm";

  // lpb_ring
  _lpb_test_function, "lpb_ring",
    // anwser, ufreq, vfreq, flux_weight,   x,   y, diameter, width
    [[     1.,    0.,    0.,     1.,  0.,  0.,     1.,    1.],
     [     1.,    0.,    0.,     1.,  0.,  0.,    1e6,  1e-6],
     [     1.,    0.,    0.,     1.,  0.,  0.,     1.,  1e-6],
     [     1.,    0.,    0.,     1.,  0.,  0.,     0.,    1.],
     [     1.,    0.,    0.,     1.,  0.,  0.,     1.,    0.],
     [     1.,   1e9,   1e9,     1.,  0.,  0.,     0.,    0.]],
     "         ufreq, vfreq, flux_weight, x, y, diameter, width";
}


func _lpb_test_function(function_name, arguments, comments)
{
  local display;
  
  // Arguments must be double (not complex)
  if (is_complex(arguments)) error, "arguments cannot be complex";
  arguments = double(arguments);
  
  // Sparse the arguments
  d = dimsof(arguments);
  if (d(1) == 1) {
    nb_tests = 1;    // arguments is a single vector.
  } else if (d(1) == 2) {
    nb_tests = d(3); // arguments is a set of vectors.
  } else {
    error, "ARGUMENTS has more than 2 dimensions !";
  }
  // We must have at least 2 arguments: answer, parameter.
  // Generally more than 4: answer, ufreq, vfreq, parameter
  nb_args = d(2);
  if (nb_args < 2) error, "Less than 2 arguments per test"
  
  write, format="\n--------- test of %s ---> diff, relative%%\n", function_name;
  write, format="%s\n", comments;

  // Check that function exists.
  if (is_void(function_name)) error, "function name is void";
  if (!symbol_exists(function_name) 
      || !is_func( (function=symbol_def(function_name)) )) {
        write, format="\tWARNING: function %s not found\n", function_name;
        return;
  }

  for (it = 1; it <= nb_tests; it++) {
  
    // Exec function, depending on the number of arguments.
    if (nb_args == 2) {
      r = function(arguments(2, it));

    } else if (nb_args == 3) {
      r = function(arguments(2, it), arguments(3, it));

    } else if (nb_args == 4) {
      r = function(arguments(2, it), arguments(3, it), arguments(4, it));
      
    } else if (nb_args == 5) {
      r = function(arguments(2, it), arguments(3, it), arguments(4, it),
                   arguments(5, it));
                   
    } else if (nb_args == 6) {
      r = function(arguments(2, it), arguments(3, it), arguments(4, it),
                   arguments(5, it), arguments(6, it));
                   
    } else if (nb_args == 7) {
      r = function(arguments(2, it), arguments(3, it), arguments(4, it),
                   arguments(5, it), arguments(6, it), arguments(7, it));

    } else if (nb_args == 8) {
      r = function(arguments(2, it), arguments(3, it), arguments(4, it),
                   arguments(5, it), arguments(6, it), arguments(7, it),
                   arguments(8, it));

    } else if (nb_args == 9) {
      r = function(arguments(2, it), arguments(3, it), arguments(4, it),
                   arguments(5, it), arguments(6, it), arguments(7, it),
                   arguments(8, it), arguments(9, it));
    } else {
      error, "more than 9 arguments not supported (yet)"
    }

    answer = arguments(1, it);
    diff = r - answer;
    if (is_complex(diff) && diff.im != 0) {     // r may be complex
      diff_string = swrite(format="%g%+gi", diff.re, diff.im);
    } else {
      diff_string = swrite(format="%g",double(diff));
    }
    if (answer != 0) {
      err = diff/answer*100.;
      if (is_complex(err) && diff.im != 0) {    // r may be complex
        err_string = swrite(format="%g%+gi%%",err.re,err.im);
      } else {
        err_string = swrite(format="%g%%",double(err));
      }
    } else {
      err_string = "";
    }    
    // Record for later display (at the end of the tests)
    line = [];
    if (nb_args > 2) {
      grow, line, swrite(format="%g,", arguments(2:-1, it));
    }
    grow, line, swrite(format="%g)", arguments(0, it)),
                " <-> ",
                swrite(format="%g", answer),
                " ---> ",
                diff_string,
                err_string;
    grow, display, [line];
  }
  _lp_print_table, display, indent=function_name+"(", no_trim=1;
}
