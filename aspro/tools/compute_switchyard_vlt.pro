pro compute_switchyard_vlt, ip
if n_elements(ip) eq 0 then ip=1 else ip=(ip<9)>1 ; ip 

;center of delay tunnel: u=52,v=-40

; telescopes U and V coordinates
N=['A0','A1','B0','B1','B2','B3','B4','B5','C0','C1','C2','C3','D0','D1','D2','E0','G0','G1','G2','H0','I1','J1','J2','J3','J4','J5','J6','K0','L0','M0','U1','U2','U3','U4']
U=[-32.001,-32.001,-23.991,-23.991,-23.991,-23.991,-23.991,-23.991,-16.002,-16.002,-16.002,-16.002,0.010,0.010,0.010,16.011,32.017,32.020,31.995,64.015,72.001,88.016,88.016,88.016,88.016,88.016,88.016,96.002,104.021,112.013,-16.000,24.000,64.0013,112.000]
V=[-48.013,-64.021,-48.019,-64.011,-72.011,-80.029,-88.013,-96.012,-48.013,-64.011,-72.019,-80.010,-48.012,-80.015,-96.012,-48.016,-48.0172,-112.010,-24.003,-48.007,-87.997,-71.992,-96.005,7.996,23.993,47.987,71.990,-48.006,-47.998,-48.000,-16.000,24.000,47.9725,8.000] 
; formula: 
; OPL0 = OPLtel + OPLm11m12 + OPLm12dl + OPLdl + OPLdlm16 + OPLm16swy + OPLswyZPD

; define the matrix of total opds (ntel,ndl)
matrix=fltarr(34,8)

; OPLtel : Optical path length between M3 and M11. 
; This is a constant different per telescope. Knowing
; the telescope the correct constant can be looked up
; in a database table.
OPLtel=replicate(18.19,34) ; was 13.2
OPLtel[30:33]=65.80 ; was 40.99
; OPLtel[31] 41.0337 (not used by Antoine?)
; add OPLtel to matrix
for i=0,7 do matrix[*,i]=OPLtel

; OPLm11-m12 : Optical path length between M11 and M12. 
; The position of M11 is given in the UV coordinate system 
; by the station coordinates plus an offset (vector) depending
; on telescope. The offset changes signs (vector changes direction)
; when a relocatable telescope is on a northern station (V>-32)
; compute m11 coordinates:
m11U=fltarr(34,8)
m11V=fltarr(34,8)
; offset first
for i=0,7 do begin 
   m11U[where(v gt -32),i] = -0.0889
   m11U[where(v le -32),i] = +0.0889
   m11U[30:33,i] = -0.4835
   m11V[where(v gt -32),i] = 0.3594
   m11V[where(v le -32),i] =-0.3594
   m11V[30:33,i] = 2.9462
endfor
; add U and V next
for i=0,7 do begin
   m11U[*,i]+=U
   m11V[*,i]+=V
endfor

;
; The M12 position is given by the same U coordinate and the V coordinate
; given by the used delay line. 

m12u=m11u

; A further complication is introduced
; by the two feeds of the delay line (see Figure A2). Telescope of
; type UT feed the REF beam of the delay line. Telescope of type
; AT feeds the ASTRO beam of the delay line. 
; IF DLV is the V position of the center of the carriage, AT tels enter
; the DL with a beam at +0.12 for DLS 1 2 5 6 and -0.12 for 3 4 7 8.
; It is the inverse for UT beams.
; Additionally, telescopes on the West require a DL on the East
; (1256), and telescopes on the East a DL West (3478)

; create a matrix for this East-West requirement:
impossibleConf=reform(replicate(1,34*8),34,8)
distrib=[0,1,4,5]
for i=0,3 do impossibleConf[where(U GT 52),distrib[i]]=0
distrib=[2,3,6,7]
for i=0,3 do impossibleConf[where(U LE 52),distrib[i]]=0

; On exit from the DLs, (M16 mirror) it is the reverse, so we compute
; in parallel the m16V position
DLV=[ -37.125, -37.875, -38.625, -39.375, -40.625, -41.375, -42.125, -42.875]
;
m12V=fltarr(34,8) 
for i=0,33 do m12v[i,*]=dlv
m16V=m12V

m12v[*,0:1]+=0.12
m12v[*,4:5]+=0.12
m12v[*,2:3]-=0.12
m12v[*,6:7]-=0.12
m12v[30:33,0:1]-=0.24
m12v[30:33,4:5]-=0.24
m12v[30:33,2:3]+=0.24
m12v[30:33,6:7]+=0.24
m16v[*,0:1]-=0.12
m16v[*,4:5]-=0.12
m16v[*,2:3]+=0.12
m16v[*,6:7]+=0.12
m16v[30:33,0:1]+=0.24
m16v[30:33,4:5]+=0.24
m16v[30:33,2:3]-=0.24
m16v[30:33,6:7]-=0.24

; add OPLm11-m12 to matrix (m12u=m11u until next earthquake):

matrix+=sqrt((m12u-m11u)^2+(m12v-m11v)^2)

; OPLm12-dl : Optical path length between M12 and Delay Line mechanical
; zero. M12 U position is given as before and DL mechanical zero U
; position is given by used delay line. The corresponding coordinates 
; can be looked up in database tables and the OPDm12-dl can be
; computed.

; FIXME valeurs de DLU taken from VLTI-ICD-ESO-15000-1918 issue 2.0
;DLU=[ 59.717, 59.717, 44.283, 44.283, 59.717, 59.717, 44.283, 44.283]
; or is it (valeur Amerand (m.Wittkowski???) )
DLU=[58.92,  58.92,  45.08,  45.08,  58.92,  58.92,  45.08,  45.08]
; add OPLm12-dl to matrix:
for i=0,7 do matrix[*,i]+=sqrt((m12u[*,i]-dlu[i])^2)

; OPLdl: Internal optical path length inside the delay line. This is a
;constant different per delay line. Value in database.

OPLdl=4.5 ; all seem the same however
;dl pos min=11/2=5.5 m ??

; add OPLdl to matrix:
matrix+=OPLdl

; OPLdl-m16 : Optical path length between Delay Line and M16.
; The delay line U coordinate is given by lookup table for given
; delay line (DLU). The M16 U coordinate is given by entry channel in 
; switch yard. The corresponding coordinates can be looked up in
; database tables and the OPDdl-m16 can be computed.

; we choose a fixed delayline->switchyard configuration: 
; (DL#,SW#): (1,1) (2,3) (3,5) (4,7) (5,2) (6,4) (7,6) (8,8)
; since it seems to be the case for AMBER. Documents show
; that probably a different switchyard setting must be used for more than
; four telescopes. Just change beam order bo below. 

beamorder=replicate(ip-1,8)
swU=[52.320, 52.56, 52.8, 53.04, 53.28, 53.25, 53.76, 54.0]

sorted_swU=fltarr(8)
for i=0,7 do sorted_swU[i]=swU[beamorder[i]]

m16u=fltarr(34,8)
for i=0,33 do m16u[i,*]=sorted_swU

; (m16v has already been computed abeamorderve)
; add OPLdl-m16 to matrix:
for i=0,7 do matrix[*,i]+=sqrt((m16u[*,i]-dlu[i])^2)

; OPLm16-swy : Optical path length between M16 and a reference plane
; at entrance of switch yard. The reference plane has a constant V 
; coordinate stored in the database with a value of -36.000. The V
; coordinate of M16 is given by delay line and feed (REF out or 
; ASTRO out) depending of type of telescope. Knowing the telescope
; type and chosen delay line the corresponding number can be looked
; up in database table and the OPDm16-swy can be computed.

swV=-36.000

; add OPLm16-swy to matrix:
matrix+=sqrt((m16v-swV)^2)

; OPLswy-ZPD : Optical path length between reference plane at entrance
; of switch yard, through the switch yard, to ZPD point in the 
; interferometric lab. This is a constant defined by the entry channel
; and the switchyard configuration for that channel. Knowing the entry
; channel and the switchyard configuration the correct constant can be
; looked up in a database table. Unit telescopes use the beam
; compressor always, others NO.
zpdU=52.000; for the record, U pos of zpd plane
;oplSwZpd=fltarr(8,3) ; 1:Direct, 2:BeamCompressor 3:BeamComp+DDL
oplSwZpd=reform([ 2.105, 1.385, 2.825, 2.105, 3.545, 2.825, 4.265, 3.545, 11.563, 12.763, 11.803, 13.003, 12.043, 13.243, 12.283, 13.483, 0, 0, 0, 0, 0, 0, 0, 0],8,3)  

addLastOpdDirect=fltarr(8) & addLastOpdBC=addLastOpdDirect
for i=0,7 do begin
 addLastOpdDirect[i]=oplSwZpd[beamorder[i],0]
 addLastOpdBC[i]=oplSwZpd[beamorder[i],1]
endfor

;add OPLswy-ZPD to matrix:
for j=0,29 do for i=0,7 do matrix[j,i]+=addLastOpdDirect[i] ; all direct
for j=30,33 do for i=0,7 do matrix[j,i]+=addLastOpdBC[i] ; UT with BC
; mask with impossible configuration matrix:

matrix[WHERE(impossibleConf EQ 0)]=!values.f_nan

 print, "#T DL1      DL2       DL3     DL4      DL5      DL6"
 format='(A2,6(X,F8.4))'
 for i=0,33 do begin
  print, format=format,N[i], matrix[i,0], matrix[i,1], matrix[i,2],  matrix[i,3],  matrix[i,4],  matrix[i,5]
 endfor
end




