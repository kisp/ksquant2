module MeasureToEnpTest where
import Measure
import MeasureToEnp
import Enp
import Test.HUnit
import Data.Ratio

mtoenp1 = TestList
           [
            Enp.Measure (4,4) [Enp.Div 4 [Enp.Chord 1 False]]
            ~=? m_to_enp (M (4,4) (60 % 1) (L 1 False))
           ,Enp.Measure (1,4) [Enp.Div 1 [Enp.Chord 1 False]]
            ~=? m_to_enp (M (1,4) (60 % 1) (L (1%4) False))           
           ,Enp.Measure (2,4) [Enp.Div 1 [Enp.Chord 1 False],
                              Enp.Div 1 [Enp.Chord 1 False,Enp.Chord 1 False]]
            ~=? m_to_enp (m (2,4) (60 % 1)
                                (d (2%4) 1
                                       [(l (1%4) False),
                                        (d (1%4) 1 [(l (1%8) False),
                                                    (l (1%8) False)])]))      
           ,Enp.Measure (2,4) [Enp.Div 1 [Enp.Chord 1 False],
                              Enp.Div 1 [Enp.Chord 1 False,Enp.Chord 1 False]]
            ~=? m_to_enp (m (2,4) (60 % 1)
                                (d (2%4) 1 [(d (1%4) 1 [(l (1%4) False)]),
                                            (d (1%4) 1 [(l (1%8) False),
                                                        (l (1%8) False)])]))           
           ]
