      **********************************************************************                        
      *                                                                   **                        
      * Programa      : VTMCCB3R                                          **                        
      * Descripción   : Programa que Cambia Valor de una Dtaara de Notas C**                        
      * Proyecto      : Menu Opciones OPI Paises                          **                        
      * Autor         : (Jortega)                                         **                        
     ** Fecha Creaciòn: 28 de Octubre de 2013                             **                        
      **********************************************************************                        
     ** Definición de Directivas de Compilación                                                     
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
                                                                                                    
     ** Definiciòn de Archivo de Pantallas                                                          
     FVtmccc0p  Cf   e             Workstn                                                          
                                                                                                    
     ** Definiciòn de Variables de Trabajo                                                          
     DPacc             s              1a   Inz(*Blanks)                                             
     DPtr              s               *   Inz(%Addr(*In))                                          
                                                                                                    
     ** Definiciòn de Estructuras de Trabajo                                                        
     DIndic            Ds                  Based(Ptr)                                               
     DSalir                            N   Overlay(Indic:3)                                         
     DGrabar                           N   Overlay(Indic:5)                                         
     DCancelar                         N   Overlay(Indic:12)                                        
                                                                                                    
     DVtdzyd           Ds                  Dtaara(VTDZYD)                                           
     DValor                           1a                                                            
                                                                                                    
     ** Definiciòn de Prototipos de Trabajo                                                         
     DPrDtaara         Pr             1a                                                            
     DPacc                            1a                                                            
                                                                                                    
     DMain             Pr                  Extpgm('VTMCCB3R')                                       
     DPcia                            3a                                                            
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
     DPcia                            3a                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       //              P r o g r a m a   P r i n c i p a l                 //                       
       //------------------------------------------------------------------//                       
      /Free                                                                                         
       Clear Rdato0;                                                                                
       Dow Salir = *Off;                                                                            
        Pacc = 'C';                                                                                 
        D0 = PrDtaara(Pacc);                                                                        
        Exfmt Rdato0;                                                                               
        *In30 = *Off;                                                                               
        If Salir = *On;                                                                             
         Leave;                                                                                     
        Endif;                                                                                      
        If Cancelar = *On;                                                                          
         Clear Rdato0;                                                                              
         Iter;                                                                                      
        Endif;                                                                                      
        If Grabar = *On;                                                                            
         Pacc = 'M';                                                                                
         D0 = PrDtaara(Pacc);                                                                       
        Endif;                                                                                      
       Enddo;                                                                                       
       *Inlr = *On;                                                                                 
      /End-Free                                                                                     
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Cambia el Valor de la Dtaara                   //                       
       //------------------------------------------------------------------//                       
     PPrDtaara         B                                                                            
     DPrDtaara         Pi             1a                                                            
     DPacc                            1a                                                            
     ** Definiciòn de Variables de Trabajo                                                          
     DValorD           s              1a                                                            
      /Free                                                                                         
       In *Lock Vtdzyd;                                                                             
       ValorD = Valor;                                                                              
       If Pacc = 'M';                                                                               
        Valor = D1;                                                                                 
        *In30 = *On;                                                                                
        Cancelar = *On;                                                                             
        D0 = *Blanks;                                                                               
        D1 = *Blanks;                                                                               
       Endif;                                                                                       
       Out Vtdzyd;                                                                                  
       Unlock Vtdzyd;                                                                               
       Return ValorD;                                                                               
      /End-Free                                                                                     
     PPrDtaara         E                                                                            
