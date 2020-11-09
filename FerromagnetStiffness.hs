module FerromagnetStiffness where

import PhysicalConsts

-- theta_0 (Stiffness [meVA^2]) 
theta_0 :: Double->Double
theta_0 Stiffness = h_plank*h_plank/e_electron/
