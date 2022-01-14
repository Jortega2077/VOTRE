***************************************************************************                         
     **                                                                   *                         
     ** Programa      : VTMCCCTR                                          *                         
     ** Descripciòn:  : Soliticar Compañia a Trabajar                     *                         
     ** Proyecto      : Menú de Opciones Opi Paises                       *                         
     ** Autor         : (Jortega)                                         *                         
     ** Fecha Creaciòn: 22 de Julio de 2011                               *                         
     **                                                                   *                         
     **********************************************************************                         
     ** Definición de Directivas de Compilación                                                     
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
     HDebug(*Yes)                                                                                   
                                                                                                    
     ** Definiciòn de Archivo de Pantallas                                                          
     FVtcpci4p  Cf   e             Workstn                                                          
                                                                                                    
     ** Definiciòn de Variables de Trabajo                                                          
     DPtr              s               *   Inz(%Addr(*In))                                          
                                                                                                    
     ** Definiciòn de Estructuras de Trabajo                                                        
                                                                                                    
     ** Definiciòn de Estructuras de Trabajo                                                        
     DIndic            Ds                  Based(Ptr)                                               
     DSalir                            N   Overlay(Indic:3)                                         
     DCancelar                         N   Overlay(Indic:12)                                        
                                                                                                    
     ** Definiciòn de Prototipos de Trabajo                                                         
     DMain             Pr                  Extpgm('Main')                                           
     DPcia                            3a                                                            
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
     DPcia                            3a                                                            
       //------------------------------------------------------------------//                       
       //              P r o g r a m a   P r i n c i p a l                 //                       
       //------------------------------------------------------------------//                       
      /Free                                                                                         
       If Pcia <= '002' Or Pcia = '100' Or Pcia = '450';                                            
        Clear Rdato0;                                                                               
        Dow Salir = *Off;                                                                           
         Exfmt Rdato0;                                                                              
         If Salir = *On;                                                                            
          Pcia = *Blanks;                                                                           
          Leave;                                                                                    
         Endif;                                                                                     
         If Cancelar = *On;                                                                         
          Clear Rdato0;                                                                             
          Iter;                                                                                     
         Endif;                                                                                     
         If PciaP = '001' Or PciaP = '002' Or                                                       
            PciaP = '100' Or PciaP = '450';                                                         
          Pcia = PciaP;                                                                             
          Leave;                                                                                    
         Endif;                                                                                     
        Enddo;                                                                                      
       Endif;                                                                                       
       *Inlr = *On;                                                                                 
      /End-Free                                                                                     
