      **********************************************************************                        
      *                                                                   **                        
      * Programa      : VTMCCCLR                                          **                        
      * Descripción   : Consulta Consecutivo, Caja Tarjeta de Inscripción **                        
      * Proyecto      : Inscripciones OPI Paises                          **                        
      * Autor         : (Jortega)                                         **                        
      * Fecha Creación: 11 de Novienbre de 2009                           **                        
      *                                                                   **                        
      **********************************************************************                        
     ** Definición de Directivas de Compilación                                                     
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
                                                                                                    
     ** Definiciòn de Archivo de Pantallas                                                          
     FVtcpci6p  Cf   e             Workstn                                                          
     F                                     Sfile(Rdatos2:Nrr)                                       
     F                                     Sfile(Rdatosc:Nrrx)                                      
                                                                                                    
     ** Definiciòn de Variables de Trabajo                                                          
     DXcia             s              3    Inz(*Blanks)                                             
     DXpais            s             20    Inz(*Blanks)                                             
     DPtr              s               *   Inz(%Addr(*In))                                          
                                                                                                    
     ** Definiciòn de Estructuras de Trabajo                                                        
     DIndic            Ds                  Based(Ptr)                                               
     DSalir                            N   Overlay(Indic:3)                                         
     DConsultar                        N   Overlay(Indic:7)                                         
     DCancelar                         N   Overlay(Indic:12)                                        
     DSflInz                           N   Overlay(Indic:70)                                        
     DSflDsp                           N   Overlay(Indic:85)                                        
     DSflDspCtl                        N   Overlay(Indic:80)                                        
     DSflClr                           N   Overlay(Indic:75)                                        
     DSflEnd                           N   Overlay(Indic:40)                                        
                                                                                                    
     D                Sds                                                                           
     DPgm                *Proc                                                                      
                                                                                                    
     ** Definiciòn de Prototipos de Trabajo                                                         
     DBorraSfl         Pr                                                                           
     DPintarSfl        Pr                                                                           
     DLlenarSflD       Pr                                                                           
     DBorraSflD        Pr                                                                           
     DPintarSflD       Pr                                                                           
     DPrLeerCed        Pr                                                                           
                                                                                                    
     DPgmPais          Pr                  Extpgm('VOTREP00/VTMCSBBR')                              
     DXcia                            3                                                             
     DXpais                          20                                                             
                                                                                                    
     DMain             Pr                  Extpgm('Main')                                           
     DPcia                            3a                                                            
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
     DPcia                            3a                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       //              P r o g r a m a   P r i n c i p a l                 //                       
       //------------------------------------------------------------------//                       
      /Free                                                                                         
         PgmPais(Pcia:Xpais);                                                                       
         BorraSfl();                                                                                
         SflDsp = *On;                                                                              
         SflEnd = *On;                                                                              
         Dow Salir = *Off;                                                                          
          Ppro = Pgm;                                                                               
          Pfecha = %Char(%Date());                                                                  
          PintarSfl();                                                                              
          If Salir = *On;                                                                           
           Leave;                                                                                   
          Endif;                                                                                    
          If Consultar = *On;                                                                       
           PrLeerCed();                                                                             
          Endif;                                                                                    
          If Cancelar = *On;                                                                        
           BorraSfl();                                                                              
           SflDsp = *On;                                                                            
           SflEnd = *On;                                                                            
           Iter;                                                                                    
          Endif;                                                                                    
         Enddo;                                                                                     
        *Inlr = *On;                                                                                
      /End-Free                                                                                     
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Borra el Sub Archivo de la Pantalla            //                       
       //------------------------------------------------------------------//                       
     PBorraSfl         B                                                                            
     DBorraSfl         Pi                                                                           
      /Free                                                                                         
       Posic = 1;                                                                                   
       Nrr = *Zeros;                                                                                
       SflClr = *On;                                                                                
       SflInz = *On;                                                                                
       Write Rcontrol2;                                                                             
       SflInz = *Off;                                                                               
       SflClr = *Off;                                                                               
      /End-Free                                                                                     
     PBorraSfl         E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Pinta el Sub Archivo de la Pantalla            //                       
       //------------------------------------------------------------------//                       
     PPintarSfl        B                                                                            
     DPintarSfl        Pi                                                                           
      /Free                                                                                         
       Ppais = Xpais;                                                                               
       SflDspCtl = *On;                                                                             
       Write Rblanco;                                                                               
       Write Rteclas2;                                                                              
       Exfmt Rcontrol2;                                                                             
       SflDspCtl = *Off;                                                                            
       *In35 = *Off;                                                                                
       *In36 = *Off;                                                                                
      /End-Free                                                                                     
     PPintarSfl        E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Lee las Cedulas Ingresadas                     //                       
       //------------------------------------------------------------------//                       
     PPrLeerCed        B                                                                            
     DPrLeerCed        Pi                                                                           
      /Free                                                                                         
       If Salir = *Off And Cancelar = *Off;                                                         
       BorraSflD();                                                                                 
        Readc Rdatos2;                                                                              
         Dow Not %Eof();                                                                            
          Select;                                                                                   
           When Pcedc <> *Zeros;                                                                    
            LlenarSflD();                                                                           
          Endsl;                                                                                    
          Pcedc = *Zeros;                                                                           
          Update Rdatos2;                                                                           
         Readc Rdatos2;                                                                             
        Enddo;                                                                                      
        If Nrrx > *Zeros;                                                                           
         PintarSflD();                                                                              
        Else;                                                                                       
         If *In36 = *Off;                                                                           
          *In35 = *On;                                                                              
         Endif;                                                                                     
        Endif;                                                                                      
       Endif;                                                                                       
      /End-Free                                                                                     
     PPrLeerCed        E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Llena el Sub Archivo del Sub File              //                       
       //------------------------------------------------------------------//                       
     PLlenarSflD       B                                                                            
     DLlenarSflD       Pi                                                                           
      ** Definición de Variable de Trabajo                                                          
     DSqlNr1           s            500a   Inz(*Blanks)                                             
      ** Definición de Estructuras de Trabajo                                                       
     DFields           Ds                                                                           
     DRecstÑ                         30    Inz(*Blanks)                                             
     DRecjcd                          6    Inz(*Blanks)                                             
     DRecncd                          6    Inz(*Blanks)                                             
                                                                                                    
      /Free                                                                                         
                                                                                                    
        // Se Consulta si el Archivo Tiene Registros por Cedula                                     
       SqlNr1 = 'Select Rerucc, Recjcd, Recncd ' +                                                  
                'From Votrea' + Pcia + '/Vtmrcdf ' +                                                
                'Where Rerucc = ' +''''+ %Trim(Pcedc)+'''';                                         
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur From :SqlNr1;                                                                   
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor1 Cursor For Cur;                                                             
                                                                                                    
       Exec Sql                                                                                     
        Open Cursor1;                                                                               
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor1 Into :Fields;                                                                 
                                                                                                    
       If Sqlcod = 100;                                                                             
         Pcedc1 = %Trim(Pcedc);                                                                     
         Pncj = 'No Asignado';                                                                      
         Pncs = 'No Asignado';                                                                      
         *in55=*on;                                                                                 
         Nrrx += 1;                                                                                 
         Posi = Nrrx;                                                                               
         Write Rdatosc;                                                                             
       else;                                                                                        
         *in55=*off;                                                                                
       ENDIF;                                                                                       
                                                                                                    
        Dow Sqlcod <> 100 And Sqlcod >= *Zeros;                                                     
         Pcedc1 = RecstÑ;                                                                           
         Pncj = Recjcd;                                                                             
         Pncs = Recncd;                                                                             
         Nrrx += 1;                                                                                 
         Posi = Nrrx;                                                                               
         Write Rdatosc;                                                                             
         *in55=*off;                                                                                
         Exec Sql                                                                                   
          Fetch Cursor1 Into :Fields;                                                               
        Enddo;                                                                                      
        SflEnd = *On;                                                                               
                                                                                                    
        Exec Sql                                                                                    
         Close Cursor1;                                                                             
                                                                                                    
        If Nrrx > *Zeros;                                                                           
         SflDsp = *On;                                                                              
        Else;                                                                                       
         SflDsp = *Off;                                                                             
         *In36 = *On;                                                                               
         SflDsp = *On;                                                                              
         SflEnd = *On;                                                                              
        Endif;                                                                                      
      /End-Free                                                                                     
     PLlenarSflD       E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Borra el Sub Archivo de la Pantalla            //                       
       //------------------------------------------------------------------//                       
     PBorraSflD        B                                                                            
     DBorraSflD        Pi                                                                           
      /Free                                                                                         
       Posic = 1;                                                                                   
       Nrrx = *Zeros;                                                                               
       SflClr = *On;                                                                                
       Write Rcontrolc;                                                                             
       SflClr = *Off;                                                                               
      /End-Free                                                                                     
     PBorraSflD        E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Pinta el Sub Archivo de la Pantalla            //                       
       //------------------------------------------------------------------//                       
     PPintarSflD       B                                                                            
     DPintarSflD       Pi                                                                           
      /Free                                                                                         
       Dow Salir = *Off;                                                                            
        Ppais = Xpais;                                                                              
        SflDspCtl = *On;                                                                            
        Write Rblanco;                                                                              
        Write Rteclas;                                                                              
        Exfmt Rcontrolc;                                                                            
        SflDspCtl = *Off;                                                                           
        If Salir = *On;                                                                             
         Leave;                                                                                     
        Endif;                                                                                      
       Enddo;                                                                                       
       Salir = *Off;                                                                                
       BorraSfl();                                                                                  
       SflDsp = *On;                                                                                
       SflEnd = *On;                                                                                
      /End-Free                                                                                     
     PPintarSflD       E                                                                            
