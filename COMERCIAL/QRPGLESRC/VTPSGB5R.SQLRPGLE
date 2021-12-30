***************************************************************************                         
     ** Programa      : VTPSGB5R                                          *                         
     ** Descripción:  : Pgm de Actualizacion de efectividad de Pedidos    *                         
     ** Proyecto      : Informe Liquidación Manual                        *                         
     ** Autor         : (AParraQ)                                         *                         
     ** Fecha Creación: Junio de 2016                                     *                         
     **********************************************************************                         
                                                                                                    
     ** Definición de Directivas de Compilación                                                     
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
                                                                                                    
     ** Parametros de Entrada al Programa                                                           
     DMain             Pr                  Extpgm('VTPSGB5R')                                       
     DP_Cia                           3                                                             
     DP_Zon                           3                                                             
     DP_Cam                           4                                                             
                                                                                                    
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
     DP_Cia                           3                                                             
     DP_Zon                           3                                                             
     DP_Cam                           4                                                             
                                                                                                    
     ** Definición estructura del sistema                                                           
     D Estado_Pgm     SDs                                                                           
     D   Nom_Pgm         *PROC                                                                      
     D   Msg_ErrID            40     46                                                             
     D   Msg_Err              91    169                                                             
     D   Nom_Job             244    253                                                             
     D   Usuario             254    263                                                             
     D   Nro_Job             264    269                                                             
     DSig_sys                195    196  0                                                          
     DMes_sys                276    277  0                                                          
     DDia_sys                278    279  0                                                          
     DAÑo_sys                280    281  0                                                          
                                                                                                    
     ** Definición de Variables de Trabajo                                                          
     DW_Codigo         s              9  0 Inz(*Zeros)                                              
     DW_Orden          S              9  0 Inz(*Zeros)                                              
     DW_Efetividad     S              1A   Inz(*Blanks)                                             
     Dw_LMGPEF         S              1A   Inz(*Blanks)                                             
                                                                                                    
     ** Definición de Prototipos de Trabajo                                                         
     DPrOrden          Pr                                                                           
     DPrProcesar       Pr                                                                           
                                                                                                    
     ** Definición de Prototipos                                                                    
     DPgmEfectividad   Pr                  Extpgm('VOTREP00/VTPEFE0R')                              
     DXCia                            3                                                             
     DXCed                            9  0                                                          
     DXOrd                            9  0                                                          
     DXEfe                            1                                                             
                                                                                                    
                                                                                                    
       //------------------------------------------------------------------//                       
       //              P r o g r a m a   P r i n c i p a l                 //                       
       //------------------------------------------------------------------//                       
      /Free                                                                                         
                                                                                                    
       If P_Cam <> '2009';                                                                          
       //Se seleccionan los datos a Procesar                                                        
       Exec SQL                                                                                     
         Declare CurEfect Cursor for                                                                
         Select LMGCOD                                                                              
         From VOTREA00/VTGLMGF                                                                      
         Where LMGCIA = :P_Cia                                                                      
           And LMGMED = :P_Cam                                                                      
           And LMGZON = :P_Zon                                                                      
           And LMGIME = ' '                                                                         
         For Update of LMGPEF;                                                                      
                                                                                                    
       Exec Sql                                                                                     
         Open CurEfect;                                                                             
                                                                                                    
       Exec Sql                                                                                     
         fetch CurEfect Into :W_Codigo;                                                             
                                                                                                    
       //se llevan los registros a la pantalla                                                      
       Dow Sqlcod <> 100;                                                                           
         PrProcesar();                                                                              
                                                                                                    
         Exec Sql                                                                                   
          Update VOTREA00/VTGLMGF                                                                   
          Set LMGPEF = :w_LMGPEF                                                                    
          Where Current Of CurEfect;                                                                
                                                                                                    
         Exec Sql                                                                                   
           Fetch CurEfect Into :W_Codigo;                                                           
       Enddo;                                                                                       
                                                                                                    
       Exec SQL                                                                                     
         close CurEfect;                                                                            
                                                                                                    
       Endif;                                                                                       
       *Inlr = *On;                                                                                 
      /End-Free                                                                                     
                                                                                                    
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que buscar los datos de cada compradora            //                       
       //------------------------------------------------------------------//                       
     PPrProcesar       B                                                                            
     DPrProcesar       Pi                                                                           
      /Free                                                                                         
                                                                                                    
        Clear W_Orden;                                                                              
        Clear w_LMGPEF;                                                                             
        Clear W_Efetividad;                                                                         
        PrOrden();                                                                                  
        If W_Orden <> 0;                                                                            
           PgmEfectividad(P_Cia:W_Codigo:W_Orden:W_Efetividad);                                     
           If W_Efetividad = 'S';                                                                   
              w_LMGPEF = 'X';                                                                       
           Else;                                                                                    
              w_LMGPEF = ' ';                                                                       
           EndIf;                                                                                   
        EndIf;                                                                                      
                                                                                                    
      /End-Free                                                                                     
     PPrProcesar       E                                                                            
                                                                                                    
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que buscar la orden                                //                       
       //------------------------------------------------------------------//                       
     PPrOrden          B                                                                            
     DPrOrden          Pi                                                                           
      /Free                                                                                         
                                                                                                    
       Exec Sql                                                                                     
         Select AdoRdÑ Into :W_Orden                                                                
         From Pardtl                                                                                
         Where AdcStÑ = :W_Codigo And AdTRTY = 'I' And                                              
               SubStr(ADDEPT, 1, 4) = :P_Cam                                                        
               Order by Rrn(Pardtl) Desc                                                            
               Fetch first 1 row only;                                                              
                                                                                                    
      /End-Free                                                                                     
     PPrOrden          E                                                                            
