***************************************************************************                         
     ** Programa      : VTPSGB3R                                          *                         
     ** Descripción:  : Insertar Registros Historia de Guías VTWAGSF      *                         
     ** Proyecto      : Plan Guías                                        *                         
     ** Autor         : Deisy Milena Cañón Roncancio  - Personalsoft S.A  *                         
     ** Fecha Creación: 11 de Abril de 2016                               *                         
     **********************************************************************                         
                                                                                                    
     ** Definición de Directivas de Compilación                                                     
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
                                                                                                    
     ** Definición de Archivo de Trabajo                                                            
     FVtwagsf   Uf   e           K Disk                                                             
                                                                                                    
     ** Definición de Variables de Trabajo                                                          
     DW_User           s             10a   Inz(*User)                                               
     DW_CampaIn        s              4a   Inz(*Blanks)                                             
     DW_Fecha          s             10a   Inz(*Blanks)                                             
     DW_FecAct         s             10a   Inz(*Blanks)                                             
     DW_HOrAct         s              8a   Inz(*Blanks)                                             
     DIzona            s              3a   Inz(*Blanks)                                             
     DIcaso            s              7a   Inz(*Blanks)                                             
     DIcedula          s             30a   Inz(*Blanks)                                             
     DIfecha           s             10a   Inz(*Blanks)                                             
     DOrespuesta       s              1a   Inz(*Blanks)                                             
     DNro              s              5  0 Inz(*Zeros)                                              
                                                                                                    
     ** Definición de Prototipos de Trabajo                                                         
     DPrInserRegis     Pr                                                                           
                                                                                                    
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pr                  Extpgm('Main')                                           
     DP_Acc                           1a                                                            
     DP_Cia                           3a                                                            
     DP_Zon                           3a                                                            
     DP_Ced                          15s 0                                                          
     DP_Sec                           3a                                                            
     DP_Obs                         250a                                                            
     DP_Usr                          10a   Const Options(*Nopass)                                   
                                                                                                    
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
     DP_Acc                           1a                                                            
     DP_Cia                           3a                                                            
     DP_Zon                           3a                                                            
     DP_Ced                          15s 0                                                          
     DP_Sec                           3a                                                            
     DP_Obs                         250a                                                            
     DP_Usr                          10a   Const Options(*Nopass)                                   
                                                                                                    
     DPgmNewPlanG      Pr                  Extpgm('VOTREP00/VTMPGB0R')                              
     DPcia                            3a                                                            
     DIzona                           3a                                                            
     DIcaso                           7a                                                            
     DIcedula                        30a                                                            
     DIfecha                         10a                                                            
     DOrespuesta                      1a                                                            
       //------------------------------------------------------------------//                       
       //              P r o g r a m a   P r i n c i p a l                 //                       
       //------------------------------------------------------------------//                       
      /Free                                                                                         
       PgmNewPlanG(P_Cia:P_Zon:Icaso:Icedula:Ifecha:Orespuesta);                                    
       If Orespuesta = 'S';                                                                         
        PrInserRegis();                                                                             
       Endif;                                                                                       
       If Orespuesta = 'N';                                                                         
        If %Scan('RUT':%Trim(P_Obs)) > *Zeros Or                                                    
           %Scan('Rut':%Trim(P_Obs)) > *Zeros Or                                                    
           %Scan('rut':%Trim(P_Obs)) > *Zeros;                                                      
         PrInserRegis();                                                                            
        Else;                                                                                       
         Nro = *Zeros;                                                                              
         Exec sql                                                                                   
          Select Count(0) Into :Nro                                                                 
          From Votrea00/Vtwagsf                                                                     
          Where Agscia = :P_Cia And                                                                 
                Agszon = :P_Zon And                                                                 
                Agsced = :P_Ced And                                                                 
                Agsobc = :P_Obs And                                                                 
                Agsfmd = :W_FecAct;                                                                 
         If Nro <= *Zeros;                                                                          
          PrInserRegis();                                                                           
         Endif;                                                                                     
        Endif;                                                                                      
       Endif;                                                                                       
       *Inlr = *On;                                                                                 
      /End-Free                                                                                     
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que buscar los datos de cada compradora            //                       
       //------------------------------------------------------------------//                       
     PPrInserRegis     B                                                                            
     DPrInserRegis     Pi                                                                           
      /Free                                                                                         
       W_FecAct = %Char(%Date());                                                                   
       W_HOrAct = %Char(%Time());                                                                   
       If P_Usr <> *Blanks;                                                                         
        W_User = P_Usr;                                                                             
       Endif;                                                                                       
       Exec sql                                                                                     
         Insert Into Votrea00/Vtwagsf Values (                                                      
          :P_Acc, :P_Cia, :P_Zon, :P_Ced, :W_CampaIn, :W_Fecha,                                     
          :W_CampaIn, :W_Fecha, :P_Obs,                                                             
          :W_FecAct, :W_HOrAct, :W_User, :P_Sec);                                                   
                                                                                                    
      /End-Free                                                                                     
     PPrInserRegis     E                                                                            
                                                                                                    
