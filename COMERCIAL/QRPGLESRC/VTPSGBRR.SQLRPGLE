      **********************************************************************                        
      ** Programa     : VTPSGBRR                                          **                        
      ** Descripciòn: : Web Guías/ Detalle Pedido-Resultado por Campaña   **                        
      ** Proyecto     : Plan Guias                                        **                        
      ** Autor        : Leonel Mauricio Parra Suárez-PersonalSoft         **                        
      ** Fecha Creaciòn: 08 de Julio de 2016                              **                        
      **********************************************************************                        
     ** Definición de Directivas de Compilación                                                     
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
                                                                                                    
      ** Definición de Archivos de Trabajo                                                          
     FLcacmp01  If   e           k Disk                                                             
                                                                                                    
     FVTPSGI7P  cf   e             Workstn                                                          
     F                                     Sfile(Pdatos:Nrr)                                        
     F                                      InfDs( infDsp )                                         
     D InfDsp          DS                                                                           
     D                                     Qualified                                                
     D   key                          1A   Overlay( InfDsp: 369 )                                   
      *Definición de Procedimientos                                                                 
     DMain             Pr                  Extpgm('Main')                                           
     DICia                            3a                                                            
     DIcamp                           4a                                                            
     DIzona                           3a                                                            
     DIsector                         1a                                                            
     DIcedulag                        9  0                                                          
     DInombreg                       30a                                                            
     DIfoco                          30a                                                            
     DIpedbas                         5  0                                                          
     DImetpeda                        5  0                                                          
     DIpedtota                        5  0                                                          
     DIcummet                         2a                                                            
     DIpedpaga                        5  0                                                          
     DIpeddela                        9  0                                                          
     DICampan                         4a                                                            
                                                                                                    
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
     DICia                            3a                                                            
     DIcamp                           4a                                                            
     DIzona                           3a                                                            
     DIsector                         1a                                                            
     DIcedulag                        9  0                                                          
     DInombreg                       30a                                                            
     DIfoco                          30a                                                            
     DIpedbas                         5  0                                                          
     DImetpeda                        5  0                                                          
     DIpedtota                        5  0                                                          
     DIcummet                         2a                                                            
     DIpedpaga                        5  0                                                          
     DIpeddela                        9  0                                                          
     DICampan                         4a                                                            
                                                                                                    
      ** Definición de Variables de Trabajo                                                         
     DPtr              s               *   Inz(%Addr(*In))                                          
     DPosiX            s              5  0 Inz(*Zeros)                                              
     DSPcia            s             10a   Inz(*Blanks)                                             
     DPres             s              2a   Inz(*Blanks)                                             
     DPciaR            s             10a   Inz(*Blanks)                                             
     DIDX              s              4  0 Inz(*Zeros)                                              
     DWtitpanta0       s             60a   Inz(*Blanks)                                             
     DWtitsec          s             60a   Inz(*Blanks)                                             
     DWtitcedg         s             60a   Inz(*Blanks)                                             
     DWtitnomg         s             60a   Inz(*Blanks)                                             
     DWtitpba          s             60a   Inz(*Blanks)                                             
     DWtitmpe          s             60a   Inz(*Blanks)                                             
     DWtitpto          s             60a   Inz(*Blanks)                                             
     DWtitcme          s             60a   Inz(*Blanks)                                             
     DWtitptp          s             60a   Inz(*Blanks)                                             
     DWtitpde          s             60a   Inz(*Blanks)                                             
     DWtitpc           s             60a   Inz(*Blanks)                                             
     DWtitcon          s             60a   Inz(*Blanks)                                             
     DWtitcedc         s             60a   Inz(*Blanks)                                             
     DWtitnomc         s             60a   Inz(*Blanks)                                             
     DWtitpcoc         s             60a   Inz(*Blanks)                                             
     DWtitppac         s             60a   Inz(*Blanks)                                             
     DWtitpdec         s             60a   Inz(*Blanks)                                             
     DWtitpcc          s             60a   Inz(*Blanks)                                             
     DWbot01           s             30a   Inz(*Blanks)                                             
     DWbot02           s             30a   Inz(*Blanks)                                             
     DWpc              s              5  0 Inz(*Zeros)                                              
     DWconsec          s              5  0 Inz(*Zeros)                                              
     DWstatus          s              1a   Inz(*Blanks)                                             
     DWdestatus        s            120a   Inz(*Blanks)                                             
     DWcia             s             10a   Inz(*Blanks)                                             
     DWzona            s              3a   Inz(*Blanks)                                             
     DWcamp            s              4a   Inz(*Blanks)                                             
                                                                                                    
     ** Definición de Estructuras de Trabajo                                                        
     DIndic            Ds                  Based(Ptr)                                               
     DSalir                            N   Overlay(Indic:3)                                         
     DCancelar                         N   Overlay(Indic:12)                                        
     DSflDsp                           N   Overlay(Indic:85)                                        
     DSflDspCtl                        N   Overlay(Indic:80)                                        
     DSflClr                           N   Overlay(Indic:75)                                        
     DSflEnd                           N   Overlay(Indic:40)                                        
                                                                                                    
     D                sDs                                                                           
     Dnomp               *Proc                                                                      
      ** Definición de Procedimientos                                                               
     DPrLlenarSfl      Pr                                                                           
     DPrBorrarSfl      Pr                                                                           
     DPrAsigCabec      pr                                                                           
      *Procedimiento Recupera Código de envio                                                       
     DCompaÑia         Pr                  Extpgm('VOTREP00/VTMCSBFR')                              
     DSPcia                          10                                                             
     DPres                            2                                                             
     DKciaR                          10                                                             
      *Procedimiento Recupera Proyección de Campaña                                                 
                                                                                                    
     DRsTramaPedidos   s                   SqlType(Result_Set_Locator)                              
      *Definición de Occurs para cargue de Información                                              
     DDsPedidos        Ds                  Occurs(999)                                              
     DGCedula                         9  0                                                          
     DGNombre                        30a                                                            
     DGPedidoC                        1a                                                            
     DGPedidoP                        1a                                                            
     DGPedidoD                        1a                                                            
     DGPC                             1a                                                            
                                                                                                    
                                                                                                    
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //                                                                  //                        
      //            P R O G R A M A    P R I N C I P A L                  //                        
      //                                                                  //                        
      //////////////////////////////////////////////////////////////////////                        
                                                                                                    
      /Free                                                                                         
       Dow Salir = *Off;                                                                            
       *in41 = *on;                                                                                 
       clear *all DsPedidos;                                                                        
       SPcia = ICia;                                                                                
       Pres = *Blanks;                                                                              
       PciaR = *Blanks;                                                                             
       CompaÑia(SPcia:Pres:PciaR);                                                                  
       WCIA  =   PciaR;                                                                             
       WZONA =   Izona;                                                                             
       WCAMP =   ICampan;                                                                           
       If Pres= 'Si';                                                                               
         Exec SQL                                                                                   
           call VOTREP00.GSL_PEDIDOSDCTOGUIAS(                                                      
                 :Wcia,:Wzona,:Wcamp,:Isector,:Icedulag,:Inombreg,:Ipedbas,                         
                 :Imetpeda,:Ipedtota,:Icummet,:Ipedpaga,:Ipeddela,:wtitpanta0,                      
                 :Wtitsec,:wtitcedg,:wtitnomg,:wtitpba,:wtitmpe,:wtitpto,                           
                 :Wtitcme,:wtitptp,:wtitpde,:wtitpc,:wtitcon,:wtitcedc,                             
                 :Wtitnomc,:wtitpcoc,:wtitppac,:wtitpdec,:wtitpcc,:wbot01,                          
                 :Wbot02,:wpc,:wconsec,:wstatus,:wdestatus);                                        
                                                                                                    
         Exec SQL                                                                                   
           Associate Result Set Locators                                                            
           (:RsTramaPedidos) With Procedure Votrep00/GSL_PedidosDctoGuias;                          
                                                                                                    
         Exec SQL                                                                                   
           Allocate Resultados Cursor                                                               
           For Result Set :RsTramaPedidos;                                                          
                                                                                                    
         Exec SQL                                                                                   
            Fetch Resultados For 900 Rows Into :DsPedidos;                                          
                                                                                                    
         Exec SQL                                                                                   
            Close Resultados;                                                                       
                                                                                                    
        PrLlenarSfl();                                                                              
        PrPintarSfl();                                                                              
        If *In03 = *On;                                                                             
         Leave;                                                                                     
        Endif;                                                                                      
        If *In12 = *On;                                                                             
         Clear Pcontrol;                                                                            
         *In12 = *Off;                                                                              
         Iter;                                                                                      
        Endif;                                                                                      
       EndIf;                                                                                       
       Enddo;                                                                                       
       *InLr = *On;                                                                                 
      /End-Free                                                                                     
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Borra el Sub Archivo de la Pantalla            //                       
       //------------------------------------------------------------------//                       
     PPrBorrarSfl      B                                                                            
     DPrBorrarSfl      Pi                                                                           
      /Free                                                                                         
       Posic = 1;                                                                                   
       Nrr = *Zeros;                                                                                
       SflClr = *On;                                                                                
       Write Pcontrol;                                                                              
       SflClr = *Off;                                                                               
      /End-Free                                                                                     
     PPrBorrarSfl      E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Llena el Sub Archivo de la Pantalla            //                       
       //------------------------------------------------------------------//                       
     PPrLlenarSfl      B                                                                            
     DPrLlenarSfl      Pi                                                                           
      /Free                                                                                         
       PrBorrarSfl();                                                                               
       If PosiX <> *Zeros;                                                                          
        Posic = PosiX;                                                                              
        PosiX = *Zeros;                                                                             
       Endif;                                                                                       
       PrAsigCabec();                                                                               
                                                                                                    
       For idx = 1 to %Elem(DsPedidos);                                                             
        %Occur(DsPedidos) = Idx;                                                                    
        IF DsPedidos.GNombre <> *Blanks Or DsPedidos.GPedidoC <> *Blanks Or                         
          DsPedidos.GPedidoP <> *Blanks Or DsPedidos.GPedidoD <> *Blanks Or                         
           DsPedidos.GCedula <> *Zeros;                                                             
         Nrr = Nrr +1;                                                                              
           PCD = DsPedidos.GCedula;                                                                 
           PNOMBRE  = DsPedidos.GNombre;                                                            
          IF DsPedidos.GPedidoC = ' ';                                                              
           PPEDCOLC = '-';                                                                          
          Else;                                                                                     
           PPEDCOLC = DsPedidos.GPedidoC;                                                           
          Endif;                                                                                    
          If DsPedidos.GPedidoP=' ';                                                                
           PPDPAG   = '-';                                                                          
          Else;                                                                                     
           PPDPAG   = DsPedidos.GPedidoP;                                                           
          EndIf;                                                                                    
          If DsPedidos.GPedidoD = ' ';                                                              
           PPEDDLT  = '-';                                                                          
          Else;                                                                                     
           PPEDDLT  = DsPedidos.GPedidoD;                                                           
          EndIf;                                                                                    
          If DsPedidos.GPC =' ';                                                                    
           PPC1     = '-';                                                                          
          Else;                                                                                     
           PPC1     = DsPedidos.GPC;                                                                
          EndIf;                                                                                    
          Posi = Nrr;                                                                               
          Write PDatos;                                                                             
        EndIF;                                                                                      
       Endfor;                                                                                      
                                                                                                    
       SflEnd = *On;                                                                                
       If Nrr > *Zeros;                                                                             
        SflDsp = *On;                                                                               
       Else;                                                                                        
        SflDsp = *Off;                                                                              
       Endif;                                                                                       
      /End-Free                                                                                     
     PPrLlenarSfl      E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Llena el Sub Archivo de la Pantalla            //                       
       //------------------------------------------------------------------//                       
     PPrAsigCabec      B                                                                            
     DPrAsigCabec      Pi                                                                           
      /Free                                                                                         
        PCAMPC  = 'Ciere a C'+ ICamp;                                                               
        Pzona   = Wzona;                                                                            
        PSECTC  = Isector;                                                                          
        PCEDUG  = Icedulag;                                                                         
        PNOMBG  = Inombreg;                                                                         
        PFOCO   = IFOCO;                                                                            
        IF Ipedbas = *Zeros;                                                                        
         PPEDBAS = '#0';                                                                            
        Else;                                                                                       
         PPEDBAS = %Trim(%Editc(Ipedbas:'Z'));                                                      
        EndIf;                                                                                      
        PCÑA1   = 'C'+Wcamp;                                                                        
        If Ipeddela = *Zeros;                                                                       
        PPEDDEL = '#0';                                                                             
        Else;                                                                                       
        PPEDDEL = %Trim(%editc(Ipeddela:'Z'));                                                      
        EndIf;                                                                                      
        PCÑA2   = 'C'+Wcamp;                                                                        
       If Imetpeda = *Zeros;                                                                        
        PMETPED = '#0';                                                                             
       Else;                                                                                        
        PMETPED = %Trim(%editc(Imetpeda:'Z'));                                                      
       EndIf;                                                                                       
        PCÑA3   = WTITPC;                                                                           
       If Wpc   = *Zeros;                                                                           
        PPC     = '#0';                                                                             
       Else;                                                                                        
        PPC     ='#'+ %Trim(%Editc(Wpc:'Z'));                                                       
       EndIf;                                                                                       
        PCÑA4    = 'C'+Wcamp;                                                                       
        If Ipedtota = *Zeros;                                                                       
         PPEDTOTA = '#0';                                                                           
        Else;                                                                                       
        PPEDTOTA =  %Trim(%editc(Ipedtota:'Z'));                                                    
        EndIf;                                                                                      
        PCÑA5    = 'C'+Wcamp;                                                                       
       If wconsec= *Zeros;                                                                          
        PCNSCTVD = '%0';                                                                            
       Else;                                                                                        
        PCNSCTVD ='%' + %Trim(%Editc(wconsec:'Z'));                                                 
       EndIf;                                                                                       
        PCUMMET  = Icummet;                                                                         
        If Ipedpaga = *zeros;                                                                       
         PPDTTPAG ='#0';                                                                            
        Else;                                                                                       
        PPDTTPAG = %Trim(%Editc(Ipedpaga:'Z'));                                                     
        EndIf;                                                                                      
                                                                                                    
      /End-Free                                                                                     
     PPrAsigCabec      E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Pinta el Sub Archivo de la Pantalla            //                       
       //------------------------------------------------------------------//                       
     PPrPintarSfl      B                                                                            
     DPrPintarSfl      Pi                                                                           
      /Free                                                                                         
       Ppgm = Nomp;                                                                                 
       Pfecha = %Char(%Date());                                                                     
       SflDspCtl = *On;                                                                             
      *Write Rteclas;                                                                               
       Write  teclas1;                                                                              
       PTITULO = '                                            C'+%Trim(WCamp)+                      
               '   C'+%Trim(WCamp)+ '  C'+%Trim(WCamp)+ ' C'+%Trim(WCamp);                          
         Exfmt Pcontrol;                                                                            
       SflDspCtl = *Off;                                                                            
      /End-Free                                                                                     
     PPrPintarSfl      E                                                                            
                                                                                                    
                                                                                                    
