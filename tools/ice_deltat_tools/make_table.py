"""Build the 2-year delta-T grid (1800.0 .. 2050.0) used by the patched ICE DELTAT.
Sources:
  1800-1972 : Swiss Ephemeris dt[] table (Astronomical Almanac K8-K9 / Stephenson-Morrison)
  1973-now  : IERS finals2000A (astropy-iers-data): dT = 32.184 + (TAI-UTC) - (UT1-UTC), value at Jan 1
  beyond    : last observed value, continued at the slope of the IERS Bulletin A prediction,
              plus the long-term tidal parabola curvature (31 s/cy^2 -> 0.0031 s/yr^2).
Output: data/dt_grid.txt  (year  dT_seconds  source)"""
import os,datetime,astropy_iers_data
here=os.path.dirname(os.path.abspath(__file__))
swe={int(a):float(b) for a,b in (l.split() for l in open(os.path.join(here,'data/swe_dt.txt')))}
D=os.path.join(os.path.dirname(astropy_iers_data.__file__),'data')
ls=[(float(l.split()[0]),int(l.split()[4])) for l in open(D+'/Leap_Second.dat') if l.strip() and not l.startswith('#')]
def taiutc(m): return [d for mm,d in ls if m>=mm][-1]
rows={}
for l in open(D+'/finals2000A.all'):
    if len(l)>=68 and l[58:68].strip():
        rows[float(l[7:15])]=(float(l[58:68]),l[57])
def mjd(y,m=1,d=1): return (datetime.date(y,m,d)-datetime.date(1858,11,17)).days
def iers_dt(m): dut,f=rows[m]; return 32.184+taiutc(m)-dut, f
last_obs=max(m for m,(v,f) in rows.items() if f=='I')
last_pred=max(rows)
y_obs=1858+ (last_obs-mjd(1858))/365.25  # approx decimal year
dt_obs,_=iers_dt(last_obs)
# slope from Bulletin A predictions (last observed -> end of predictions)
dt_pred,_=iers_dt(last_pred)
slope=(dt_pred-dt_obs)/((last_pred-last_obs)/365.25)
ACC=0.0031
def extrap(y):
    t=y-y_obs; return dt_obs+slope*t+ACC*t*t
grid=[]
for y in range(1800,2051,2):
    if y<1973: v,src=swe[y],'AA/SwissEph'
    elif mjd(y) in rows and rows[mjd(y)][1]=='I': v,src=iers_dt(mjd(y))[0],'IERS observed'
    elif mjd(y) in rows: v,src=iers_dt(mjd(y))[0],'IERS Bulletin A prediction'
    else: v,src=extrap(y),'extrapolated'
    grid.append((y,v,src))
with open(os.path.join(here,'data/dt_grid.txt'),'w') as f:
    f.write(f'# last IERS observed MJD {last_obs:.0f} (~{y_obs:.2f}) dT={dt_obs:.3f}s; BullA slope {slope:+.3f} s/yr; accel {ACC} s/yr^2\n')
    for y,v,s in grid: f.write(f'{y} {v:8.3f} {s}\n')
print(open(os.path.join(here,'data/dt_grid.txt')).read())
