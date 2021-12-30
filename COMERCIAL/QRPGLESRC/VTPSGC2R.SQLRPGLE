      ***********************************************************************                       
      ** Programa      : VTPSGC2R                                          **                       
      ** Descripciòn   : Verifica Cupo Base de Líderes                     **                       
      ** Proyecto      : Plan de Guías                                     **                       
      ** Autor         : (JOrtega)                                         **                       
      ** Fecha Creación: 20 de Septiembre de 2017                          **                       
      ***********************************************************************                       
      ** Definición de Directivas de Compilación                                                    
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
                                                                                                    
      ** Definición de Variables de Trabajo                                                         
     DNum              s              9  0 Inz(*Zeros)                                              
     DIndex0           s              5U 0                                                          
     DIcaso            s              7a   Inz(*Blanks)                                             
     DIfecha           s             10a   Inz(*Blanks)                                             
     DWfecha           s             10a   Inz(*Blanks)                                             
     DIcedula          s             30a   Inz(*Blanks)                                             
     DOrespuesta       s              1a   Inz(*Blanks)                                             
     DP_CedA           s              9a   Inz(*Blanks)                                             
     DP_Sec            s              3a   Inz(*Blanks)                                             
                                                                                                    
     ** Definiciòn de Prototipos de Trabajo                                                         
     DDsArrayCampos    Ds                  Dim(15000) Qualified                                     
     DGldcia                          3a                                                            
     DGldzon                          3a                                                            
     DGldsct                          1a                                                            
     DGldcte                         15  0                                                          
                                                                                                    
     DElements         s              5I 0 Inz(%Elem(DsArrayCampos))                                
                                                                                                    
     DPrValidar        Pr              n                                                            
     DIciaQ                           3a                                                            
     DIzonaQ                          3a                                                            
     DIsectorQ                        1a                                                            
     DIcedulaQ                       15s 0                                                          
                                                                                                    
     DPgmNewPlanG      Pr                  Extpgm('VOTREP00/VTMPGB0R')                              
     DPcia                            3a                                                            
     DIzona                           3a                                                            
     DIcaso                           7a                                                            
     DIcedula                        30a                                                            
     DIfecha                         10a                                                            
     DOrespuesta                      1a                                                            
                                                                                                    
     DPgmPedidosIniciales...                                                                        
     D                 Pr                  Extpgm('VOTREP00/VTPSGB8L')                              
     DP_Cia                           3a                                                            
     DP_CedA                          9a                                                            
     DP_Zon                           3a                                                            
     DP_Sec                           3a                                                            
       //------------------------------------------------------------------//                       
       //              P r o g r a m a   P r i n c i p a l                 //                       
       //------------------------------------------------------------------//                       
      /Free                                                                                         
       Wfecha = %Char(%Date()-%Days(5));                                                            
       Clear DsArrayCampos;                                                                         
       Exec Sql                                                                                     
        Declare Cur0 Cursor For                                                                     
        Select Gldcia, Gldzon, Gldsct, Gldcte                                                       
        From Votrea00/Vtmgldf                                                                       
        Where Gldest = ' ' And                                                                      
              Substr(Gldfin, 7, 4)||'-'|| Substr(Gldfin,                                            
              4, 2)||'-'||Substr(Gldfin, 1, 2) >= :Wfecha;                                          
                                                                                                    
       Exec Sql                                                                                     
        Open Cur0;                                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Fetch From Cur0 For :Elements Rows Into :DsArrayCampos;                                     
                                                                                                    
       Num = SqlEr3;                                                                                
                                                                                                    
       Exec Sql                                                                                     
        Close Cur0;                                                                                 
                                                                                                    
       For Index0 = 1 To Num By 1;                                                                  
        Icaso = *Blanks;                                                                            
        Icedula = %Trim(%Editc(DsArrayCampos(Index0).Gldcte:'Z'));                                  
        Ifecha = *Blanks;                                                                           
        Orespuesta = *Blanks;                                                                       
        PgmNewPlanG(DsArrayCampos(Index0).Gldcia:DsArrayCampos(Index0).Gldzon:                      
                    Icaso:Icedula:Ifecha:Orespuesta);                                               
        If Orespuesta = 'S';                                                                        
         If PrValidar(DsArrayCampos(Index0).Gldcia:                                                 
                      DsArrayCampos(Index0).Gldzon:                                                 
                      DsArrayCampos(Index0).Gldsct:                                                 
                      DsArrayCampos(Index0).Gldcte) = *Off;                                         
          P_CedA = %Trim(%Char(DsArrayCampos(Index0).Gldcte));                                      
          P_Sec = %Trim(DsArrayCampos(Index0).Gldsct);                                              
          PgmPedidosIniciales(DsArrayCampos(Index0).Gldcia:P_CedA:                                  
                              DsArrayCampos(Index0).Gldzon:P_Sec);                                  
         Endif;                                                                                     
        Endif;                                                                                      
       EndFor;                                                                                      
                                                                                                    
       Return;                                                                                      
      /End-Free                                                                                     
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Valida si la Guia Tiene ya el Cupo Base        //                       
       //------------------------------------------------------------------//                       
     PPrValidar        B                                                                            
     DPrValidar        Pi              n                                                            
     DIciaQ                           3a                                                            
     DIzonaQ                          3a                                                            
     DIsectorQ                        1a                                                            
     DIcedulaQ                       15s 0                                                          
      ** Definición de Variables de Trabajo                                                         
     DHnro             s              5  0 Inz(*Zeros)                                              
     DPsw              s               n   Inz(*Off)                                                
      /Free                                                                                         
       Exec Sql                                                                                     
        Select Count(0) Into :Hnro                                                                  
        From Votrea00/Vtmnpif                                                                       
        Where NpiCia = :IciaQ And                                                                   
              NpiZna = :IzonaQ And                                                                  
              Npicdg = :IcedulaQ And                                                                
              NpiSec = :IsectorQ And                                                                
              NpiEst = ' ';                                                                         
                                                                                                    
       If Hnro > *Zeros;                                                                            
        Psw = *On;                                                                                  
       Endif;                                                                                       
                                                                                                    
       Return Psw;                                                                                  
                                                                                                    
      /End-Free                                                                                     
     PPrValidar        E                                                                            
