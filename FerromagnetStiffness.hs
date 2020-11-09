module FerromagnetStiffness where

import PhysicalConsts

-- (theta_0 stiffness [meVA^2]) [rad]
theta_0 :: Double->Double
theta_0 stiffness = (h_plank/2/pi)**2/2/m_neutron/(toJoul stiffness)/1e-20  -- 1e-20 - Angstrom^2


-- convert Joul to meV
toMev x = 1000*x/e_electron
-- convert meV to Joul
toJoul x = x*e_electron/1000
