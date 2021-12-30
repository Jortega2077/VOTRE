      **********************************************************************                        
      ** Programa     : VTPSGBPR                                          **                        
      ** Descripciòn: : Web Guías/ Resultados Guías por Campaña           **                        
      ** Proyecto     : Plan Guias                                        **                        
      ** Autor        : Leonel Mauricio Parra Suárez-PersonalSoft         **                        
      ** Fecha Creaciòn: 05 de Julio de 2016                              **                        
      **********************************************************************                        
     ** Definición de Directivas de Compilación                                                     
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
                                                                                                    
      ** Definición de Archivos de Trabajo                                                          
     FVTPSGI5P  cf   e             Workstn                                                          
     F                                     Sfile(Pdatos:Nrr)                                        
     F                                     Sfile(Pdatos2:Nrr1)                                      
     F                                     Sfile(Pdatos3:Nrr2)                                      
                                                                                                    
      *Definición de Procedimientos                                                                 
     DMain             Pr                  Extpgm('Main')                                           
     DIcia                            3a                                                            
     DIcampa                          4a                                                            
     DIzona                           3a                                                            
     DInombrel                       30a                                                            
                                                                                                    
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
     DIcia                            3a                                                            
     DIcampa                          4a                                                            
     DIzona                           3a                                                            
     DInombrel                       30a                                                            
                                                                                                    
      ** Definición de Variables de Trabajo                                                         
     DPtr              s               *   Inz(%Addr(*In))                                          
     DPosiX            s              5  0 Inz(*Zeros)                                              
     DPsw              s              1a   Inz('N')                                                 
     DPsw1             s              1a   Inz('N')                                                 
     DSPcia            s             10a   Inz(*Blanks)                                             
     DPres             s              2a   Inz(*Blanks)                                             
     DPciaR            s             10a   Inz(*Blanks)                                             
     DIDX              s              4  0 Inz(*Zeros)                                              
     DWcia             s             10a   Inz(*Blanks)                                             
     DWzona            s              3a   Inz(*Blanks)                                             
     DWcamp            s              4a   Inz(*Blanks)                                             
     DWnrosec          s              2a   Inz(*Blanks)                                             
     DWfoco            s             30a   Inz(*Blanks)                                             
     DWtitpanta0       s             60a   Inz(*Blanks)                                             
     DWtitpanta1       s             60a   Inz(*Blanks)                                             
     DWtitpanta2       s             60a   Inz(*Blanks)                                             
     DWtitpanta3       s             60a   Inz(*Blanks)                                             
     DWtitsec          s             60a   Inz(*Blanks)                                             
     DWtitced          s             60a   Inz(*Blanks)                                             
     DWtitnom          s             60a   Inz(*Blanks)                                             
     DWtitped          s             60a   Inz(*Blanks)                                             
     DWtitmpta         s             60a   Inz(*Blanks)                                             
     DWtitptoa         s             60a   Inz(*Blanks)                                             
     DWtitcpea         s             60a   Inz(*Blanks)                                             
     DWtitptpa         s             60a   Inz(*Blanks)                                             
     DWtititoa         s             60a   Inz(*Blanks)                                             
     DWtitidea         s             60a   Inz(*Blanks)                                             
     DWtitpdea         s             60a   Inz(*Blanks)                                             
     DWtitalaa         s             60a   Inz(*Blanks)                                             
     DWtitmpe          s             60a   Inz(*Blanks)                                             
     DWtitpto          s             60a   Inz(*Blanks)                                             
     DWtitcpe          s             60a   Inz(*Blanks)                                             
     DWtitito          s             60a   Inz(*Blanks)                                             
     DWtitpde          s             60a   Inz(*Blanks)                                             
     DWtitala          s             60a   Inz(*Blanks)                                             
     DWstatus          s              1a   Inz(*Blanks)                                             
     DWdestatus        s            120a   Inz(*Blanks)                                             
     DPCamE            s              4a   Inz(*Blanks)                                             
     DPCamR            s              4a   Inz(*Blanks)                                             
     DPOper            s              1a   Inz(*Blanks)                                             
     DPnume            s              3a   Inz(*Blanks)                                             
     DWCampaAnt        s              4a   Inz(*Blanks)                                             
     DWusua            s             10a   Inz(*User)                                               
     DXcia             s              3a   Inz(*Blanks)                                             
     DXcampa           s              4a   Inz(*Blanks)                                             
     DXzona            s              3a   Inz(*Blanks)                                             
     DXSector          s             10a   Inz(*Blanks)                                             
     DXCedula          s              9  0 Inz(*Zeros)                                              
     DXNombre          s             30a   Inz(*Blanks)                                             
     DXFOCO            s             30a   Inz(*Blanks)                                             
     DXPedidoB         s              5  0 Inz(*Zeros)                                              
     DXMetPedA         s              5  0 Inz(*Zeros)                                              
     DXPedTotA         s              5  0 Inz(*Zeros)                                              
     DXCumPedA         s              2a   Inz(*Blanks)                                             
     DXPedPagA         s              5  0 Inz(*ZEROS)                                              
     DXInsTotA         s              5  0 Inz(*Zeros)                                              
     DXInsDelA         s              9  0 Inz(*Zeros)                                              
     DXPedDelA         s              9  0 Inz(*Zeros)                                              
     DXAlasA           s             10a   Inz(*Blanks)                                             
     DWNrr             s              9  0 Inz(*Zeros)                                              
                                                                                                    
     ** Definición de Estructuras de Trabajo                                                        
     DIndic            Ds                  Based(Ptr)                                               
     DSalir                            N   Overlay(Indic:3)                                         
     DCancelar                         N   Overlay(Indic:12)                                        
     DMas_Inf                          N   Overlay(Indic:11)                                        
     DSflDsp                           N   Overlay(Indic:85)                                        
     DSflDspCtl                        N   Overlay(Indic:80)                                        
     DSflClr                           N   Overlay(Indic:75)                                        
     DSflEnd                           N   Overlay(Indic:40)                                        
                                                                                                    
     D                sDs                                                                           
     Dnomp               *Proc                                                                      
      ** Definición de Procedimientos                                                               
     DPrLlenarSfl      Pr                                                                           
     DPrBorrarSfl      Pr                                                                           
     DPrPintarSfl      Pr                                                                           
     DPrCllDllSec      Pr                                                                           
      *Procedimiento Recupera Código de envio                                                       
     DCompaÑia         Pr                  Extpgm('VOTREP00/VTMCSBFR')                              
     DSPcia                          10                                                             
     DPres                            2                                                             
     DKciaR                          10                                                             
      *Procedimiento Recupera Proyección de Campaña                                                 
     DProCampa         Pr                  Extpgm('VOTREP00/VTMCSBKR')                              
     DPcia                            3a                                                            
     DPCamE                           4a                                                            
     DPCamR                           4a                                                            
     DPOper                           1a                                                            
     DPnume                           3a                                                            
                                                                                                    
      *Procedimiento Ejecuta Detalle por sector                                                     
     DPgmDetalleXSec   Pr                  Extpgm('VOTREP00/VTPSGBQR')                              
     DXcia                            3a                                                            
     DXcampa                          4a                                                            
     DXzona                           3a                                                            
     DXSector                        10a                                                            
     DXCedula                         9  0                                                          
     DXNombre                        30a                                                            
     DXFOCO                          30a                                                            
     DXPedidoB                        5  0                                                          
     DXMetPedA                        5  0                                                          
     DXPedTotA                        5  0                                                          
     DXCumPedA                        2a                                                            
     DXPedPagA                        5  0                                                          
     DXInsTotA                        5  0                                                          
     DXInsDelA                        9  0                                                          
     DXPedDelA                        9  0                                                          
     DXAlasA                         10a                                                            
                                                                                                    
     DRsTramaResult    s                   SqlType(Result_Set_Locator)                              
      *Definición de Occurs para cargue de Información                                              
     DDsResultados     Ds                  Occurs(100)                                              
     DGSector                        10a                                                            
     DGCedula                         9  0                                                          
     DGNombre                        30a                                                            
     DGPedidoB                        5  0                                                          
     DGMetPedA                        5  0                                                          
     DGPedTotA                        5  0                                                          
     DGCumPedA                        2a                                                            
     DGPedPagA                        5  0                                                          
     DGInsTotA                        5  0                                                          
     DGInsDelA                        9  0                                                          
     DGPedDelA                        9  0                                                          
     DGAlasA                         10a                                                            
     DGMetPed                         5  0                                                          
     DGPedTot                         5  0                                                          
     DGCumPed                         2a                                                            
     DGInsTot                         5  0                                                          
     DGPedDel                         9  0                                                          
     DGAlas                          10a                                                            
                                                                                                    
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //                                                                  //                        
      //            P R O G R A M A    P R I N C I P A L                  //                        
      //                                                                  //                        
      //////////////////////////////////////////////////////////////////////                        
                                                                                                    
      /Free                                                                                         
       Dow Salir = *Off;                                                                            
         clear *all DsResultados;                                                                   
         SPcia = ICia;                                                                              
         Pres = *Blanks;                                                                            
         PciaR = *Blanks;                                                                           
         CompaÑia(SPcia:Pres:PciaR);                                                                
         WCIA  =   PciaR;                                                                           
         WZONA =   Izona;                                                                           
         WCAMP =   Icampa;                                                                          
         If Pres= 'Si';                                                                             
            Exec SQL                                                                                
             Call VOTREP00.GSL_RESULTADOSGUIAS(                                                     
             :Wcia,:Wzona,:Wcamp,:Wnrosec,:Wfoco,:Wtitpanta0,:Wtitpanta1,                           
             :Wtitpanta2,:Wtitpanta3,:Wtitsec,:Wtitced,:Wtitnom,:Wtitped,                           
             :Wtitmpta,:Wtitptoa,:Wtitcpea,:Wtitptpa,:Wtititoa,:Wtitidea,                           
             :Wtitpdea,:Wtitalaa,:Wtitmpe,:Wtitpto,:Wtitcpe,:Wtitito,:Wtitpde,                      
             :Wtitala,:Wstatus,:Wdestatus);                                                         
                                                                                                    
            Exec SQL                                                                                
                 Associate Result Set Locators                                                      
                 (:RsTramaResult) With Procedure Votrep00.Gsl_ResultadosGuias;                      
                                                                                                    
            Exec SQL                                                                                
                 Allocate Resultados Cursor                                                         
                 For Result Set :RsTramaResult;                                                     
                                                                                                    
            Exec SQL                                                                                
                 Fetch Resultados For 100 Rows Into :DsResultados;                                  
                                                                                                    
            Exec SQL                                                                                
                 Close Resultados;                                                                  
                                                                                                    
            PrLlenarSfl();                                                                          
            PrPintarSfl();                                                                          
            If *In03 = *On;                                                                         
             Leave;                                                                                 
            Endif;                                                                                  
            If *In12 = *On;                                                                         
             Clear Pcontrol;                                                                        
             Clear Pcontrol2;                                                                       
             Clear Pcontrol3;                                                                       
             Psw = 'S';                                                                             
             Psw1 = 'S';                                                                            
             Mas_Inf = *On;                                                                         
             *In12 = *Off;                                                                          
             Iter;                                                                                  
            Endif;                                                                                  
            If Nrr > *Zeros;                                                                        
             Readc Pdatos;                                                                          
             Dow Not %Eof ();                                                                       
              Select;                                                                               
               When Pop = 'X';                                                                      
                WNrr = Nrr ;                                                                        
                PrCllDllSec();                                                                      
                PosiX = Posi;                                                                       
                Clear Pop;                                                                          
              EndSl;                                                                                
              Pop = *Blanks;                                                                        
              Update Pdatos;                                                                        
              Readc Pdatos;                                                                         
             Enddo;                                                                                 
            Endif;                                                                                  
                                                                                                    
            If Nrr1> *Zeros;                                                                        
             Readc Pdatos2;                                                                         
             Dow Not %Eof ();                                                                       
              Select;                                                                               
               When Pop = 'X';                                                                      
               WNrr = Nrr1;                                                                         
                PrCllDllSec();                                                                      
                PosiX = Posi;                                                                       
                Clear Pop;                                                                          
              EndSl;                                                                                
              Pop = *Blanks;                                                                        
              Update Pdatos2;                                                                       
              Readc Pdatos2;                                                                        
             Enddo;                                                                                 
            Endif;                                                                                  
            If Nrr2> *Zeros;                                                                        
             Readc Pdatos3;                                                                         
             Dow Not %Eof ();                                                                       
              Select;                                                                               
               When Pop = 'X';                                                                      
               WNrr = Nrr2;                                                                         
                PrCllDllSec();                                                                      
                PosiX = Posi;                                                                       
                Clear Pop;                                                                          
              EndSl;                                                                                
              Pop = *Blanks;                                                                        
              Update Pdatos3;                                                                       
              Readc Pdatos3;                                                                        
             Enddo;                                                                                 
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
       Nrr1= *Zeros;                                                                                
       Nrr2= *Zeros;                                                                                
       SflClr = *On;                                                                                
       Write Pcontrol;                                                                              
       Write Pcontrol2;                                                                             
       Write Pcontrol3;                                                                             
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
                                                                                                    
        PCamR = *Blanks;                                                                            
        POper = '-';                                                                                
        Pnume = '1';                                                                                
        If ICAMPA = '2010';                                                                         
           Pnume = '2';                                                                             
        EndIf;                                                                                      
        ProCampa(ICIA:ICAMPA:PCamR:POper:Pnume);                                                    
        WCampaAnt = PCamR;                                                                          
                                                                                                    
       Pcampc = Wtitpanta2;                                                                         
       Pnombl = Inombrel;                                                                           
       Pnrosec= Wnrosec;                                                                            
       Pzona  = Wzona;                                                                              
       Pfoco  = Wfoco;                                                                              
       For idx = 1 to %Elem(DsResultados);                                                          
        %Occur(DsResultados) = Idx;                                                                 
        If GSector <> *Blanks;                                                                      
            Nrr = Nrr + 1;                                                                          
            Posi = Nrr;                                                                             
         If GSector = 'Sin Guía  ';                                                                 
           Psect   = 'SG';                                                                          
           *In39 = *On;                                                                             
         Else;                                                                                      
           *In39 = *Off;                                                                            
           Psect   = GSector;                                                                       
         EndIf;                                                                                     
         Pcd     = GCedula;                                                                         
         Pnombre = GNombre;                                                                         
         Ppdbase = GPedidoB;                                                                        
         Pmtpeda = GMetPedA;                                                                        
         Ppdtta  = GPedTotA;                                                                        
         Pcmpeda = GCumPedA;                                                                        
         Ppdpaga = GPedPagA;                                                                        
         Pinstta = GInsTotA;                                                                        
          Nrr1= Nrr1+ 1;                                                                            
          Posi = Nrr;                                                                               
          Write PDatos;                                                                             
         Pinsdela= GInsDelA;                                                                        
         Ppeddela= GPedDelA;                                                                        
         Palasa  = GAlasA  ;                                                                        
         Pmtpdcir= GMetPed ;                                                                        
          Nrr2= Nrr2+ 1;                                                                            
          Write PDatos2;                                                                            
         Ppedtot = GPedTot ;                                                                        
         Pcmped = GCumPed;                                                                          
         Pinstt = GInsTot;                                                                          
         Ppeddel = GPedDel ;                                                                        
         Palas   = GAlas   ;                                                                        
          Write PDatos3;                                                                            
          Posi = Nrr;                                                                               
        EndIf;                                                                                      
       Endfor;                                                                                      
                                                                                                    
       SflEnd = *On;                                                                                
       If Nrr > *Zeros;                                                                             
        SflDsp = *On;                                                                               
       Else;                                                                                        
        SflDsp = *Off;                                                                              
       Endif;                                                                                       
       If Nrr1 > *Zeros;                                                                            
        SflDsp = *On;                                                                               
       Else;                                                                                        
        SflDsp = *Off;                                                                              
       Endif;                                                                                       
       If Nrr2 > *Zeros;                                                                            
        SflDsp = *On;                                                                               
       Else;                                                                                        
        SflDsp = *Off;                                                                              
       Endif;                                                                                       
      /End-Free                                                                                     
     PPrLlenarSfl      E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Pinta el Sub Archivo de la Pantalla            //                       
       //------------------------------------------------------------------//                       
     PPrPintarSfl      B                                                                            
     DPrPintarSfl      Pi                                                                           
      /Free                                                                                         
       Ppgm = Nomp;                                                                                 
       //PUsua = Wusua;                                                                             
       Pfecha = %Char(%Date());                                                                     
       SflDspCtl = *On;                                                                             
       Write Rteclas;                                                                               
       If Psw = 'S' And Psw1='S' And Mas_Inf = *On;                                                 
          Psw = 'N';                                                                                
          Psw1= 'N';                                                                                
          Mas_Inf = *Off;                                                                           
       EndIf;                                                                                       
       If Mas_Inf = *Off And Psw = 'N' And Psw1='N';                                                
          Mas_Inf = *On;                                                                            
          PTITULO = 'Op                                         C' +                                
                   %Trim(WCampaAnt)+' C'+%Trim(WCampaAnt)+ ' C'+                                    
                   %Trim(WCampaAnt)+                                                                
                   '   C'+   %Trim(WCampaAnt)+ ' C'+%Trim(WCampaAnt);                               
          Exfmt Pcontrol;                                                                           
       ElseIf Mas_Inf = *On And Psw = 'N' And Psw1='N';                                             
          Mas_Inf = *Off;                                                                           
          PTITULO2= 'Op                                   C'+%Trim(WCampaAnt)+                      
                   '    C'+%Trim(WCampaAnt)+'       C'+%Trim(WCampaAnt)+                            
                   '     C'+%Trim(WCamp);                                                           
          Exfmt Pcontrol2;                                                                          
          If Mas_Inf = *On;                                                                         
             Psw = 'S';                                                                             
          EndIf;                                                                                    
       ElseIf Mas_Inf = *On And Psw = 'S' And Psw1='N';                                             
          PTITULO3= 'Op                                    C'+%Trim(WCamp)+                         
                   '  C'+ %Trim(WCamp)+'  C'+%Trim(WCamp)+'   C'+%Trim(WCamp)+                      
                   '   C'+ %Trim(WCamp);                                                            
          Exfmt Pcontrol3;                                                                          
          Psw1= 'S';                                                                                
          Mas_Inf = *On;                                                                            
       EndIf;                                                                                       
       SflDspCtl = *Off;                                                                            
      /End-Free                                                                                     
     PPrPintarSfl      E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento de invocacion de Consulta de Detalle por Sector    //                       
       //------------------------------------------------------------------//                       
     PPrCllDllSec      B                                                                            
     DPrCllDllSec      Pi                                                                           
      /Free                                                                                         
                                                                                                    
         %Occur(DsResultados) =WNrr;                                                                
         Xcia     = ICIA;                                                                           
         Xcampa   = Icampa;                                                                         
         Xzona    = IZONA;                                                                          
         XSector  = GSector;                                                                        
         XCedula  = GCedula;                                                                        
         XNombre  = GNombre;                                                                        
         XFOCO    = PFoco;                                                                          
         XPedidoB = GPedidoB;                                                                       
         XMetPedA = GMetPedA;                                                                       
         XPedTotA = GPedTotA;                                                                       
         XCumPedA = GCumPedA;                                                                       
         XPedPagA = GPedPagA;                                                                       
         XInsTotA = GInsTotA;                                                                       
         XInsDelA = GInsDelA;                                                                       
         XPedDelA = GPedDelA;                                                                       
         XAlasA   = GAlasA;                                                                         
         PgmDetalleXSec(XCIA:Xcampa:XZONA:XSector:XCedula:XNombre                                   
         :Xfoco:XPedidoB:XMetPedA:XPedTotA:XCumPedA:XPedPagA                                        
         :XInsTotA:XInsDelA:XPedDelA:XAlasA);                                                       
                                                                                                    
      /End-Free                                                                                     
     PPrCllDllSec      E                                                                            
