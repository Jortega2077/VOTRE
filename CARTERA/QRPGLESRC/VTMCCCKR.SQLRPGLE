***************************************************************************                         
     **                                                                   *                         
     ** Programa      : VTMCCCKR                                          *                         
     ** Descripciòn:  : Suma la campaña + 1                               *                         
     ** Autor         : (Dbetancour)                                      *                         
     ** Fecha Creaciòn: 28 de Febrero 2020                               *                          
     **                                                                   *                         
     **********************************************************************                         
     ** Definición de Directivas de Compilación                                                     
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
     HDebug(*Yes)                                                                                   
                                                                                                    
     ** Definiciòn de Archivo de Pantallas                                                          
                                                                                                    
     ** Definiciòn de Variables de Trabajo                                                          
        Dcl-s POper    Char(1);                                                                     
        Dcl-s Pnume    Char(3);                                                                     
        Dcl-s ciaEnvio Char(4);                                                                     
        Dcl-s campa5   Char(4);                                                                     
        Dcl-s CampaP   Char(4);                                                                     
                                                                                                    
     ** Definiciòn de Estructuras de Trabajo                                                        
                                                                                                    
     ** Definiciòn de Estructuras de Trabajo                                                        
                                                                                                    
        Dcl-Pr Pgmcampa Extpgm('VOTREP00/VTMCSBKR');                                                
          Pcia  Char(3);                                                                            
          PCamE Char(4);                                                                            
          PCamR Char(4);                                                                            
          POper Char(1);                                                                            
          Pnume Char(3);                                                                            
        End-Pr;                                                                                     
                                                                                                    
     ** Definiciòn de Prototipos de Trabajo                                                         
     DMain             Pr                  Extpgm('Main')                                           
     DPcia                            3a                                                            
     Dcampa                           4a                                                            
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
     DPcia                            3a                                                            
     Dcampa                           4a                                                            
       //------------------------------------------------------------------//                       
       //              P r o g r a m a   P r i n c i p a l                 //                       
       //------------------------------------------------------------------//                       
      /Free                                                                                         
         Exec Sql                                                                                   
         SELECT MAX(CAMCAM) INTO :CAMPA                                                             
         FROM VOTREP00/VTCTLCF0                                                                     
         WHERE CAMCIA=:PCIA;                                                                        
                                                                                                    
         ciaEnvio=pcia;                                                                             
         CampaP=Campa;                                                                              
         campa5=' ';                                                                                
         POper='+';                                                                                 
         pnume='1';                                                                                 
         PgmCampa(ciaEnvio:CampaP:campa5:POper:Pnume);                                              
         campa=campa5;                                                                              
                                                                                                    
       *Inlr = *On;                                                                                 
      /End-Free                                                                                     
