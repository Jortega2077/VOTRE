      **********************************************************************                        
      ** Programa     : VTPSGBGR                                          **                        
      ** Descripciòn: : Selección de Zona - Resumen de Descuentos (Resul- **                        
      **                tados de Guias)                                   **                        
      ** Proyecto     : Plan Guias                                        **                        
      ** Autor        : AParraQ                                           **                        
      ** Fecha Creaciòn: Diciembre de 2016                                **                        
      **********************************************************************                        
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
                                                                                                    
     ** Definición de Archivo de Trabajo                                                            
     FLslman01  If   e           K Disk    UsrOpn                                                   
                                                                                                    
     **Defición de Archivo de Pantalla                                                              
     FVTPSGIGP  Cf   e             Workstn                                                          
     F                                     Sfile(RDatos:Nrr)                                        
                                                                                                    
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pr                  Extpgm('VTPSGBGR')                                       
     DP_Cia                           3a                                                            
     DP_Camp                          4a                                                            
                                                                                                    
     DMain             Pi                                                                           
     DP_cia                           3a                                                            
     DP_Camp                          4a                                                            
                                                                                                    
                                                                                                    
      *Definición de Estructuras                                                                    
     D                sDs                                                                           
     Dnomp               *Proc                                                                      
                                                                                                    
      *Programas Externos                                                                           
     DLibreria         Pr                  Extpgm('VOTREP00/VTMGOB2R')                              
     D$Prms                           1                                                             
     D$Lbrs                          10                                                             
     D$Optn                          10                                                             
     D$Refe                          10                                                             
                                                                                                    
      //Descripcion Pais                                                                            
     DPgmPais          Pr                  Extpgm('VOTREP00/VTMCSBBR')                              
     DXcia                            3                                                             
     DXpais                          20                                                             
                                                                                                    
      //Programa que suma/resta Campañas                                                            
     DProCampa         Pr                  Extpgm('VOTREP00/VTMCSBKR')                              
     DPcia                            3a                                                            
     DPCamE                           4a                                                            
     DPCamR                           4a                                                            
     DPOper                           1a                                                            
     DPnume                           3a                                                            
                                                                                                    
      //Programa Detalle de Resultados                                                              
     DPgmResultados    Pr                  Extpgm('VOTREP00/VTPSGBPR')                              
     DXcia                            3                                                             
     DXcam                            4                                                             
     DXzon                            3                                                             
     DXnom                           30                                                             
                                                                                                    
      *Definición de Procedimientos                                                                 
     DPrPintarSfl      Pr                                                                           
     DPrLlenarSfl      Pr                                                                           
     DPrBorrarSfl      Pr                                                                           
     DPrLeerOpcion     Pr                                                                           
     DPrLibrerias      Pr                                                                           
                                                                                                    
     ** Definiciòn de Estructuras de Trabajo                                                        
     DIndic            Ds                  Based(Ptr)                                               
     DSalir                            N   Overlay(Indic:3)                                         
     DCancelar                         N   Overlay(Indic:12)                                        
     DOcultarCmp                       N   Overlay(Indic:33)                                        
     DPosicionarZ                      N   Overlay(Indic:34)                                        
     DPMetas                           N   Overlay(Indic:35)                                        
     DSflDsp                           N   Overlay(Indic:85)                                        
     DSflDspCtl                        N   Overlay(Indic:80)                                        
     DSflClr                           N   Overlay(Indic:75)                                        
     DSflEnd                           N   Overlay(Indic:70)                                        
                                                                                                    
                                                                                                    
      *Definición de variables de trabajo                                                           
     DPosix            s              4  0 Inz(*Zeros)                                              
     DI                s              4  0 Inz(*Zeros)                                              
     DPtr              s               *   Inz(%Addr(*In))                                          
     D$coma            s              4a   Inz('''')                                                
     DPOper            S              1A                                                            
     DPnume            S              3A   Inz('1')                                                 
     DwCampAnt         S              4A   Inz(*Blanks)                                             
     DWsql             s            500a   Inz(*Blanks)                                             
     D$Prms            S              1                                                             
     D$Lbrs            S             10    Inz(' ')                                                 
     D$Optn            S             10    Inz('*FIRST')                                            
     D$Refe            S             10    Inz(' ')                                                 
                                                                                                    
                                                                                                    
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //                                                                  //                        
      //            P R O G R A M A    P R I N C I P A L                  //                        
      //                                                                  //                        
      //////////////////////////////////////////////////////////////////////                        
      /Free                                                                                         
                                                                                                    
       $Prms = 'A';                                                                                 
       PrLibrerias();                                                                               
       If Not %open(Lslman01);                                                                      
          Open Lslman01;                                                                            
       EndIf;                                                                                       
                                                                                                    
       Pfecha = %Char(%Date());                                                                     
       Ppgm = Nomp;                                                                                 
       PgmPais(P_cia:PNomp);                                                                        
                                                                                                    
       //Obtiene campaña anterior                                                                   
       POper = '-';                                                                                 
       ProCampa(P_cia:P_Camp:wCampAnt:POper:Pnume);                                                 
       If wCampAnt = '2009';                                                                        
        wCampAnt = '2008';                                                                          
       Endif;                                                                                       
                                                                                                    
       Dow Not Salir;                                                                               
         PrPintarSfl();                                                                             
         If Salir;                                                                                  
           Leave;                                                                                   
         EndIf;                                                                                     
         If Cancelar;                                                                               
           Iter;                                                                                    
         EndIf;                                                                                     
       EndDo;                                                                                       
                                                                                                    
       If %open(Lslman01);                                                                          
          Close Lslman01;                                                                           
       EndIf;                                                                                       
       $Prms = 'R';                                                                                 
       PrLibrerias();                                                                               
                                                                                                    
       *Inlr = *On;                                                                                 
      /End-Free                                                                                     
                                                                                                    
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Pinta el Sub Archivo de la Pantalla            //                       
       //------------------------------------------------------------------//                       
     PPrPintarSfl      B                                                                            
     DPrPintarSfl      Pi                                                                           
      /Free                                                                                         
                                                                                                    
       Dow Not Salir;                                                                               
         PrLlenarSfl();                                                                             
         If Posix > *Zeros;                                                                         
           Posic = Posix;                                                                           
           Posix = *Zeros;                                                                          
         EndIf;                                                                                     
                                                                                                    
         SflDspCtl = *On;                                                                           
         Write RTeclas;                                                                             
         Exfmt RControl;                                                                            
         SflDspCtl = *Off;                                                                          
                                                                                                    
         If Salir;                                                                                  
           Leave;                                                                                   
         Endif;                                                                                     
                                                                                                    
         If SZona <> *BLANKS;                                                                       
           Iter;                                                                                    
         EndIf;                                                                                     
                                                                                                    
         If Nrr > *Zeros;                                                                           
           PrLeerOpcion();                                                                          
         Endif;                                                                                     
       Enddo;                                                                                       
      /End-Free                                                                                     
     PPrPintarSfl      E                                                                            
                                                                                                    
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Llena el Sub Archivo del Sub File              //                       
       //------------------------------------------------------------------//                       
     PPrLlenarSfl      B                                                                            
     DPrLlenarSfl      Pi                                                                           
      /Free                                                                                         
                                                                                                    
       PrBorrarSfl();                                                                               
                                                                                                    
       //Se seleccionan los datos a mostrar en el SubFile                                           
       Exec SQL                                                                                     
        Declare CursorDesc Cursor for                                                               
        Select SPGZON, Sum(SPGVDS), Max(SPGFBT)                                                     
        From votrea00/vtmspgf                                                                       
        Where spgcia = :P_Cia                                                                       
          And Spgcam = :wCampAnt                                                                    
          And Spgccr In (42, 43)                                                                    
        Group by spgzon                                                                             
        Order by spgzon                                                                             
        For Read Only;                                                                              
                                                                                                    
       Exec SQL                                                                                     
        Open CursorDesc;                                                                            
                                                                                                    
       Exec SQL                                                                                     
        Fetch CursorDesc Into :PZona, :PDescuento, :PFecha;                                         
                                                                                                    
       //se llevan los registros a la pantalla                                                      
       Dow Sqlcod = 0;                                                                              
         Pop  = *Blanks;                                                                            
         Chain (PZona) Lslman01;                                                                    
         If %Found(Lslman01);                                                                       
            PNomL = SMNAME;                                                                         
         EndIf;                                                                                     
         If SZona = *Blanks Or SZona = PZona;                                                       
            Nrr += 1;                                                                               
            Posi = Nrr;                                                                             
            Write RDatos;                                                                           
         EndIf;                                                                                     
         Exec Sql                                                                                   
          Fetch CursorDesc Into :PZona, :PDescuento, :PFecha;                                       
       Enddo;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close CursorDesc;                                                                           
                                                                                                    
       // Limpia posicionador por zona                                                              
       SZona = *Blanks;                                                                             
                                                                                                    
       SflEnd = *On;                                                                                
       If Nrr > *Zeros;                                                                             
         SflDsp = *On;                                                                              
       Else;                                                                                        
         SflDsp = *Off;                                                                             
       EndIf;                                                                                       
                                                                                                    
      /End-Free                                                                                     
     PPrLlenarSfl      E                                                                            
                                                                                                    
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que se Encarga de Borrar Archivo de Control        //                       
       //------------------------------------------------------------------//                       
     PPrBorrarSfl      B                                                                            
     DPrBorrarSfl      Pi                                                                           
      /Free                                                                                         
       Posic = 1;                                                                                   
       Nrr = *Zeros;                                                                                
       SflClr = *On;                                                                                
       Write RControl;                                                                              
       SflClr = *Off;                                                                               
      /End-Free                                                                                     
     PPrBorrarSfl      E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Lee las Opciones de la Pantalla                //                       
       //------------------------------------------------------------------//                       
     PPrLeerOpcion     B                                                                            
     DPrLeerOpcion     Pi                                                                           
      /Free                                                                                         
       If Nrr > *Zeros;                                                                             
         Readc RDatos;                                                                              
         Dow Not %Eof();                                                                            
           If Pop = 'X';                                                                            
              PgmResultados(P_Cia:P_Camp:Pzona:PNomL);                                              
              Posix = Posi;                                                                         
           EndIf;                                                                                   
           Pop = *Blanks;                                                                           
           Update RDatos;                                                                           
          Readc RDatos;                                                                             
         Enddo;                                                                                     
       EndIf;                                                                                       
      /End-Free                                                                                     
     PPrLeerOpcion     E                                                                            
                                                                                                    
                                                                                                    
       //------------------------------------------------------------------//                       
       // Adiciona Libreria                                                //                       
       //------------------------------------------------------------------//                       
     PPrLibrerias      B                                                                            
     DPrLibrerias      Pi                                                                           
      /Free                                                                                         
                                                                                                    
       // Adiciona o Remueve PrLibrerias de Trabajo                                                 
       $Lbrs = 'OPF' + %Trim(P_Cia);                                                                
       Callp Libreria($Prms:$Lbrs:$Optn:$Refe);                                                     
                                                                                                    
      /End-Free                                                                                     
     PPrLibrerias      E                                                                            
