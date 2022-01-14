***************************************************************************                         
     **                                                                   *                         
     ** Programa      : VTMCCC0R                                          *                         
     ** Descripciòn:  : Consulta de Conceptos Contables Cartera           *                         
     ** Proyecto      : Menú de Opciones Opi Paises                       *                         
     ** Autor         : (Jortega)                                         *                         
     ** Fecha Creaciòn: 23 de Diciembre de 2008                           *                         
     **                                                                   *                         
     **********************************************************************                         
     ** Definición de Directivas de Compilación                                                     
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
                                                                                                    
     **Definiciòn de Archivos de Trabajo                                                            
     FPgladj    Uf a e           k Disk                                                             
     ** Definiciòn de Archivo de Pantallas                                                          
     FVtcpci0p  Cf   e             Workstn                                                          
     F                                     Sfile(Rdatos:Nrr)                                        
                                                                                                    
     ** Definiciòn de Variables de Trabajo                                                          
     DXcia             s              3    Inz(*Blanks)                                             
     DXpais            s             20    Inz(*Blanks)                                             
     DPtr              s               *   Inz(%Addr(*In))                                          
                                                                                                    
     ** Definiciòn de Estructuras de Trabajo                                                        
     DIndic            Ds                  Based(Ptr)                                               
     DSalir                            N   Overlay(Indic:3)                                         
     DSflDsp                           N   Overlay(Indic:85)                                        
     DSflDspCtl                        N   Overlay(Indic:80)                                        
     DSflClr                           N   Overlay(Indic:75)                                        
     DSflEnd                           N   Overlay(Indic:40)                                        
                                                                                                    
     ** Definiciòn de Estructuras de Trabajo                                                        
     DFields           Ds                                                                           
     DQGjdele                              Like(Gjdele)                                             
     DQGjadpt                              Like(Gjadpt)                                             
     DQGjcmt1                              Like(Gjcmt1)                                             
                                                                                                    
     ** Definiciòn de Prototipos de Trabajo                                                         
     DLlenarSfl        Pr                                                                           
     DBorraSfl         Pr                                                                           
     DPintarSfl        Pr                                                                           
                                                                                                    
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
         Dow Salir = *Off;                                                                          
          Pfecha = %Char(%Date());                                                                  
          PintarSfl();                                                                              
          If Nrr <= *Zeros And Salir = *Off;                                                        
           *In38 = *On;                                                                             
           Exfmt Rdato1;                                                                            
           *In38 = *Off;                                                                            
           Iter;                                                                                    
          Endif;                                                                                    
          If Salir = *On;                                                                           
           Leave;                                                                                   
          Endif;                                                                                    
         Enddo;                                                                                     
        *Inlr = *On;                                                                                
      /End-Free                                                                                     
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Llena el Sub Archivo del Sub File              //                       
       //------------------------------------------------------------------//                       
     PLlenarSfl        B                                                                            
     DLlenarSfl        Pi                                                                           
      /Free                                                                                         
       BorraSfl();                                                                                  
       Exec Sql                                                                                     
        Declare C1 Scroll Cursor For                                                                
        Select Gjdele, Gjadpt, Gjcmt1                                                               
        From Pgladj                                                                                 
        Order By Gjadpt;                                                                            
                                                                                                    
       Exec Sql                                                                                     
        Open C1;                                                                                    
                                                                                                    
       Exec Sql                                                                                     
        Fetch C1 Into :Fields;                                                                      
                                                                                                    
        Dow Sqlcod <> 100;                                                                          
         If QGjdele = *Blanks;                                                                      
          Pest = 'ACTIVO';                                                                          
         Else;                                                                                      
          Pest = 'INACTIVO';                                                                        
         Endif;                                                                                     
         Pcod = QGjadpt;                                                                            
         Pnom = QGjcmt1;                                                                            
         Nrr += 1;                                                                                  
         Posi = Nrr;                                                                                
         Write Rdatos;                                                                              
       Exec Sql                                                                                     
        Fetch C1 Into :Fields;                                                                      
        Enddo;                                                                                      
                                                                                                    
       Exec Sql                                                                                     
        Close C1;                                                                                   
                                                                                                    
        SflEnd = *On;                                                                               
        If Nrr > *Zeros;                                                                            
         SflDsp = *On;                                                                              
        Else;                                                                                       
         SflDsp = *Off;                                                                             
        Endif;                                                                                      
      /End-Free                                                                                     
     PLlenarSfl        E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Borra el Sub Archivo de la Pantalla            //                       
       //------------------------------------------------------------------//                       
     PBorraSfl         B                                                                            
     DBorraSfl         Pi                                                                           
      /Free                                                                                         
       Posic = 1;                                                                                   
       Nrr = *Zeros;                                                                                
       SflClr = *On;                                                                                
       Write Rcontrol;                                                                              
       SflClr = *Off;                                                                               
      /End-Free                                                                                     
     PBorraSfl         E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Pinta el Sub Archivo de la Pantalla            //                       
       //------------------------------------------------------------------//                       
     PPintarSfl        B                                                                            
     DPintarSfl        Pi                                                                           
      /Free                                                                                         
       LlenarSfl();                                                                                 
       Ppais = Xpais;                                                                               
       SflDspCtl = *On;                                                                             
       Write Rteclas;                                                                               
       Exfmt Rcontrol;                                                                              
       SflDspCtl = *Off;                                                                            
      /End-Free                                                                                     
     PPintarSfl        E                                                                            
