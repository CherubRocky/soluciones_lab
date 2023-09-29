-- Moreno Delgado Luis Ángel y Zaldivar Alanis Rodrigo --                               
data Bin a = Cero | Uno | All (Bin a) (Bin a) deriving Show                             
                                                                                        
numElem :: Bin a -> Int                                                                 
numElem Cero = 0                                                                        
numElem Uno = 0                                                                         
numElem (All b a) = 1 + numElem a                                                       
                                                                                        
binToNat :: Bin a -> Int                                                                
binToNat Cero = 0                                                                       
binToNat Uno = 1                                                                        
binToNat (All a b) = ((binToNat a) * 2) ^ (numElem (All a b)) + binToNat b
