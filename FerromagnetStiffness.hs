module FerromagnetStiffness where

import PhysicalConsts

-- (theta_0 stiffness [meVA^2]) [rad]
theta_0 :: Double->Double
theta_0 stiffness = (h_plank/2/pi)**2/2/m_neutron/(_meVToJoul stiffness)/1e-20  -- 1e-20 - Angstrom^2

-- (theta_C2 stiffnedd [meVA^2] filed [T] wavelenght [Angstrom]) [rad^2]   // theta_C^2 without gap 
theta_C2 :: Double->Double->Double->Double
theta_C2 stiffness field lambda =
  (theta_0 stiffness)**2 - (theta_0 stiffness)*(_TeslaTomeV field)/(neutronEnergy'meV lambda)
theta_C stiffness field lambda = sqrt (theta_C2 stiffness field lambda)

-- (theta_C2'gap stiffnedd [meVA^2] filed [T] gap [meV] wavelenght [Angstrom]) [rad^2]   // theta_C^2 with gap 
theta_C2'gap :: Double->Double->Double->Double->Double
theta_C2'gap stiffness field gap lambda =
  theta_C2 stiffness (field+(_meVToTesla gap)) lambda
theta_C'gap stiffness field gap lambda = sqrt (theta_C2'gap stiffness field gap lambda)


-- Incedent neutron energy form wavelenght
-- (neutronEnegrhy lambda [Angstrom]) [Joul]
neutronEnergy'Joul lambda = h_plank**2/lambda**2/2/m_neutron/1e-20
neutronEnergy'meV lambda = _JoulTomev (neutronEnergy'Joul lambda)   -- in meV
neutronEnergy'K lambda = (neutronEnergy'Joul lambda)/k_boltzman  -- in Kelvin


-- convert Joul to meV
_JoulTomev x = 1000*x/e_electron
_meVToJoul x = x/_JoulTomev 1

-- convert Tesla magnet field to Joul
_TeslaToJoul x = g_factor*u_bor*x
_JoulToTesla x = x/_TeslaToJoul 1

-- convert Tesla to meV
_TeslaTomeV x = (_JoulTomev (_TeslaToJoul x))
_meVToTesla x = x/_TeslaTomeV 1
