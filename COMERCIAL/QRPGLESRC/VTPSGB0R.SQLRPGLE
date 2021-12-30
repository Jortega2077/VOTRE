***************************************************************************                         
     **                                                                   *                         
     ** Programa      : VTPSGB0R                                          *                         
     ** Descripciòn:  : Poster Dinero a Cartera Guías en Linea            *                         
     ** Proyecto      : Nuevo Plan de Guías                               *                         
     ** Autor         : (JOrtega)                                         *                         
     ** Fecha Creaciòn: 09 de Marzo de 2016                               *                         
     **                                                                   *                         
     **********************************************************************                         
     ** Descripciòn: : Insertar información a Archovo Votrea00/VtwLpgf   **                         
     **                Archivo de control de pagos Gías                  **                         
     ** Autor        : Leonel Mauricio Parra Suárez-PersonalSoft         **                         
     ** Fecha Modificación: 30 de Marzo de 2017                          **                         
     **********************************************************************                         
      * Autor         : Leonel Mauricio Parra Suárez - PersonalSoft S.A.S.*                         
      * Responsable   : (Jortega)                                         *                         
      * Fecha Creaciòn: Abril 11 de 2018                                  *                         
      * Descripción   : Descomentarear prototipo PrActualizarVtedndf,     *                         
      *                 Actualiza Archivo Vtedndf                         *                         
      *********************************************************************                         
     ** Definición de Directivas de Compilación                                                     
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
                                                                                                    
     ** Definicion de Archivos a Utilizar                                                           
     FLorhdr07  If   e           k Disk                                                             
     FVtwcabf2  If a e           k Disk    UsrOpn                                                   
     FVtmcpgf   Uf   e           k Disk                                                             
     FVttgzcf   Uf a e           k Disk                                                             
     FVtwpgef   Uf a e           k Disk                                                             
     FVtmipgf   If   e           k Disk                                                             
     FVtmbnsf   If   e           k Disk                                                             
     FVtmtpcf   If   e           k Disk                                                             
     FLmllst01  If   e           k Disk                                                             
     FVtmspgf   If   e           k Disk                                                             
     FVtmntef   If   e           k Disk                                                             
     FVtmrtgf   If   e           k Disk                                                             
     FVtwpgif   If a e           k Disk                                                             
     FVttpprf   If a e           k Disk                                                             
                                                                                                    
     ** Definiciòn de Archivo de Pantallas                                                          
     FVtgslilp  Cf   e             Workstn                                                          
     F                                     Sfile(Rdatos:Nrr)                                        
     F                                     Sfile(Rdatos1:Nrr1)                                      
     F                                     Sfile(Rdatos2:Nrr2)                                      
                                                                                                    
     ** Definiciòn de Variables de Trabajo                                                          
     DW_ConseEpi       s              3S 0 Inz(001)                                                 
     DPagoG            s           2000a    Inz(*Blanks)                                            
     DXXpsw            s              1    Inz('N')                                                 
     DRTotales         s             14s 2                                                          
     DXcia             s              3    Inz(*Blanks)                                             
     DXpais            s             20    Inz(*Blanks)                                             
     DSqlStr           s           1000a                                                            
     DSqlStr1          s           1000a                                                            
     DPtr              s               *   Inz(%Addr(*In))                                          
     DWnombre          s             30    Inz(*Blanks)                                             
     DTfecha           s             10a   Inz(*Blanks)                                             
     DqFecha           s             10a   Inz(*Blanks)                                             
     DFecha            s               d   Datfmt(*Usa) Inz(*Sys)                                   
     DPosix            s              4s 0 Inz(*Zeros)                                              
     DQNumero          s              5s 0 Inz(*Zeros)                                              
     DBlan             s              1a   Inz(' ')                                                 
     DWordc            c                   Const(888888888)                                         
     DWsald            s             11  2 Inz(*Zeros)                                              
     DKord             s              9s 0 Inz(*Zeros)                                              
     DConse            s              6s 0 Inz(*Zeros)                                              
     DConsea           s              6a   Inz(*Blanks)                                             
     DConseb           s              5a   Inz(*Blanks)                                             
     DYcia             s              3a   Inz(*Blanks)                                             
     DTced             s             30a   Inz(*Blanks)                                             
     DTcns             s              9a   Inz(*Blanks)                                             
     DIcedula          s              9s 0 Inz(*Zeros)                                              
     DPCamE            s              4a   Inz(*Blanks)                                             
     DPCamEE           s              4a   Inz(*Blanks)                                             
     DPCamR            s              4a   Inz(*Blanks)                                             
     DPCamR1           s              4a   Inz(*Blanks)                                             
     DPOper            s              1a   Inz(*Blanks)                                             
     DPnume            s              3a   Inz(*Blanks)                                             
     DNrrAux           s              5i 0 Inz(*Zeros)                                              
     DWsql             s           2000A                                                            
     DWsql2            s            500A                                                            
     DWsql3            s            500A                                                            
     DWsql4            s            500A                                                            
     DWsql5            s            500A                                                            
     DWsql6            s            500A                                                            
     DWsql7            s            500A                                                            
     DWsql8            s            500A                                                            
     DW_Cont           s              9S 0 Inz(*Zeros)                                              
     DW_ValT           s             12S 0 Inz(*Zeros)                                              
     DW_ValTra         s             11S 2 Inz(*Zeros)                                              
     D$COMA            c                   ''''                                                     
     DWfiltroEst       s            100a                                                            
     DBancoProv        s             20a                                                            
     DFecha3           s             10A                                                            
     DFecha1           s              8S 0                                                          
     DFecha2           s             10A                                                            
     DEnddate          s               D                                                            
     DStrdate          s               D                                                            
     DFechaGen         s              8A                                                            
     DTipoTrasn        s              2S 0                                                          
     DW_Conse          s              3S 0 Inz(001)                                                 
     DWlib             s             10a                                                            
     DWPrime           s              1A                                                            
     DW_INdL           s              1A                                                            
     DPtipoW           s              1a   Inz('C')                                                 
     DPcedula          s             15  0 Inz(*Zeros)                                              
     DPvalor           s             14  2 Inz(*Zeros)                                              
     DPcampa           s              4    Inz(*Blanks)                                             
     DDias_sum         s              1  0 Inz(*Zeros)                                              
     DW_IndFtp         s              1A   Inz('N')                                                 
     DW_ZonaAux        s              3A                                                            
     DW_Mensaje        s           1000A                                                            
     DIfecha           s             10a   Inz(*Blanks)                                             
     DOrespuesta       s              1a   Inz(*Blanks)                                             
     DOfecha           s             10a   Inz(*Blanks)                                             
     DPccte            s             30a   Inz(*Blanks)                                             
     DIcaso            s              7a   Inz(*Blanks)                                             
     DSpgcami          s              4a   Inz(*Blanks)                                             
     DTnroP            S              9  0 Inz(0)                                                   
                                                                                                    
     DPres             s              2A                                                            
     DPremitente       s            200A                                                            
     DPdestinatario    s            200A                                                            
     DPRemMostrar      s           1000A                                                            
     DPAsunto          s           1000A                                                            
     DPmensajeE        s          32767A                                                            
     DPReplyto         s          10000A                                                            
     DPcc              s          10000A                                                            
     DPcco             s          10000A                                                            
     DSoapr            s          32767A                                                            
     DZonaAux          s              3A                                                            
     DUfecha           s              8    Inz(*Blanks)                                             
     DUdia             s             24    Inz(*Blanks)                                             
     DHrol             s              2a   Inz(*Blanks)                                             
     DHfec             s             10a   Inz(*Blanks)                                             
     DHtexto           s            200a   Inz(*Blanks)                                             
     DHtextoC          s            200a   Inz(*Blanks)                                             
     DOnum             s              4  0 Inz(*Zeros)                                              
     DOnumX            s              4  0 Inz(*Zeros)                                              
     DooPOS            s              4  0 Inz(*Zeros)                                              
     DSpgcedAux        s              9  0 Inz(*Zeros)                                              
     DXnro             s              4  0 Inz(*Zeros)                                              
     DYnro             s              4  0 Inz(*Zeros)                                              
                                                                                                    
      //Variables para FTP                                                                          
     DPindIFS          s              1a   Inz(*Blanks)                                             
     DPcodApli         s              3  0 Inz(*Zeros)                                              
     DPdesPli          s              2a   Inz(*Blanks)                                             
     DPextension       s             10a   Inz(*Blanks)                                             
     DParchivoOri      s            100a   Inz(*Blanks)                                             
     DParchivoDes      s            100a   Inz(*Blanks)                                             
     DPlibre           s            200a   Inz(*Blanks)                                             
     DSpgzoni          s              3a   Inz(*Blanks)                                             
      *Variable temporales almacenar Cia y zona, para recuperar región y aignar VtwLpgf             
     Dw_TempCia        s              3a   Inz(*Blanks)                                             
     Dw_TempZon        s              3a   Inz(*Blanks)                                             
     Dw_AcuDtl         s             15  2                                                          
     DWadtndÑ          s             30                                                             
     DEvalor           s             11  2                                                          
                                                                                                    
     ** Definiciòn de Estructuras de Trabajo                                                        
     DIndic            Ds                  Based(Ptr)                                               
     DSalir                            N   Overlay(Indic:3)                                         
     DPosteo                           N   Overlay(Indic:5)                                         
     DEnviarPag                        N   Overlay(Indic:6)                                         
     DFiltrar                          N   Overlay(Indic:8)                                         
     DCambiarFechas                    N   Overlay(Indic:9)                                         
     DSflDsp                           N   Overlay(Indic:85)                                        
     DSflDspCtl                        N   Overlay(Indic:80)                                        
     DSflClr                           N   Overlay(Indic:75)                                        
     DSflEnd                           N   Overlay(Indic:40)                                        
                                                                                                    
     ** Definiciòn de Estructuras de Trabajo                                                        
     DFields           Ds                                                                           
     DQSpgcia                         3a                                                            
     DQSpgzon                         3a                                                            
     DQSpgcam                         4a                                                            
     DQSpgfds                        10a                                                            
     DQSpginp                         1a                                                            
     DTotal                          14s 2 Inz(*Zeros)                                              
                                                                                                    
     DFields1          Ds                                                                           
     DRSpgcia                         3a                                                            
     DRSpgsec                         1a                                                            
     DRSpgtrr                         1a                                                            
     DRSpgzon                         3a                                                            
     DRSpgcam                         4a                                                            
     DRSpgvps                        14s 2                                                          
     DRSpgfbt                        10a                                                            
     DRSpgced                        15s 0                                                          
     DRSpginp                         1a                                                            
                                                                                                    
     DFields2        e Ds                  Extname(Vtmspgf:Regspg)                                  
     D                                     Prefix(x)                                                
                                                                                                    
     DFields3          Ds                                                                           
     DESpgcia                         3a                                                            
     DESpgzon                         3a                                                            
     DESpgcam                         4a                                                            
     DESpgfds                        10a                                                            
     DESpginp                         1a                                                            
     DETotal                         14s 2 Inz(*Zeros)                                              
     DESpgced                        15S 0                                                          
                                                                                                    
     DFields6        e Ds                  Extname(pardtl:PARDTLF1)                                 
     D                                     Prefix(Q)                                                
     DMyDtaara         Ds                  Dtaara(Vtdpnc)                                           
     DProceso                  1      1a                                                            
                                                                                                    
      //Dtaara con el consecutivo                                                                   
     DMyDtaara1        Ds                  Dtaara(VTDTCPG)                                          
     DNomb_arc                       11A                                                            
     D                Sds                                                                           
     DJob                    244    253                                                             
     DUser                   254    263                                                             
     DSig_sys                195    196  0                                                          
     DMes_sys                276    277  0                                                          
     DDia_sys                278    279  0                                                          
     DAÑo_sys                280    281  0                                                          
      *Estructura contiene campos de archivo de contro de pagos Gías                                
     DFields4        e Ds                  Extname(VtwLpgf:RegLpg)                                  
     D                                     Prefix(Q)                                                
     DFields5        e Ds                  Extname(VtwLpgf:RegLpg)                                  
     D                                     Prefix(y)                                                
     ** Definiciòn de Prototipos de Trabajo                                                         
     DPrDtaara         Pr              n                                                            
     DPrFTP            Pr                                                                           
     DLlenarSfl        Pr                                                                           
     DBorraSfl         Pr                                                                           
     DPintarSfl        Pr                                                                           
     DLlenarSflD       Pr                                                                           
     DLlenarSflDE      Pr                                                                           
     DBorraSflD        Pr                                                                           
     DBorraSflDE       Pr                                                                           
     DPintarSflD       Pr                                                                           
     DPrNombreGuia     Pr            30                                                             
     DBorraGuidelPosteo...                                                                          
     D                 Pr                                                                           
     DGenerarposteoCartera...                                                                       
     D                 Pr                                                                           
     DGenerarposteoCartera1...                                                                      
     D                 Pr                                                                           
     DW_indicador                     1A                                                            
     DPrPremisa        Pr                                                                           
     DAfectarCartera   Pr                                                                           
     DAfectarCartera1  Pr                                                                           
     DW_IndLLamado                    1A                                                            
     DPrBuscarOrden    Pr                                                                           
     DPrSaldo          Pr                                                                           
     DPrConsecutivo    Pr                                                                           
     DPrActualizar     Pr                                                                           
     DPrGrabarCierre   Pr                                                                           
     DPrCerrarPost     Pr                                                                           
     DPrPremiosEspeciales...                                                                        
     D                 Pr                                                                           
     DPrActFechas      Pr                                                                           
     DPrInfPagEfec     Pr                                                                           
     DFiltro           Pr                                                                           
     DConsultar        Pr                                                                           
     DPrDtaara1        Pr                                                                           
                                                                                                    
     DPgmPais          Pr                  Extpgm('VOTREP00/VTMCSBBR')                              
     DXcia                            3                                                             
     DXpais                          20                                                             
                                                                                                    
     DConversion       Pr                  Extpgm('VOTREP00/VTMCSBCR')                              
     DYcia                            3                                                             
     DTced                           30                                                             
     DTcns                            9                                                             
                                                                                                    
     ** Programa de ftp                                                                             
     DPgmFtp           Pr                  Extpgm('VOTREP00/VTMGOI2R')                              
     DPindIFS                         1a                                                            
     DPcodApli                        3  0                                                          
     DPdesPli                         2a                                                            
     DPextension                     10a                                                            
     DParchivoOri                   100a                                                            
     DParchivoDes                   100a                                                            
     DPlibre                        200a                                                            
                                                                                                    
     DQcmdexc          Pr                  Extpgm('QCMDEXC')                                        
     D                             3000a   Const Options(*Varsize)                                  
     D                               15p 5 Const                                                    
     D                                3a   Const Options(*Nopass)                                   
                                                                                                    
                                                                                                    
     DProCampa         Pr                  Extpgm('VOTREP00/VTMCSBKR')                              
     DPcia                            3a                                                            
     DPCamE                           4a                                                            
     DPCamR                           4a                                                            
     DPOper                           1a                                                            
     DPnume                           3a                                                            
     DPrCampaÑa        Pr             4a                                                            
     DPrActualizarVtedndf...                                                                        
     D                 Pr                                                                           
                                                                                                    
     DEnviaCorreo      Pr                  Extpgm('VOTREP00/VTEPWB3R')                              
     DPcia                            3A                                                            
     DPres                            2A                                                            
     DPremitente                    200A                                                            
     DPdestinatario                 200A                                                            
     DPRemMostrar                  1000A                                                            
     DPAsunto                      1000A                                                            
     DPmensajeE                   32767A                                                            
     DPReplyto                    10000A                                                            
     DPcc                         10000A                                                            
     DPcco                        10000A                                                            
     DSoapr                       32767A                                                            
                                                                                                    
     DPgmMensajeTexto  Pr                  Extpgm('VOTREP00/VTNRBGVR')                              
     DPtipoW                          1a                                                            
     DPcia                            3a                                                            
     DPcedula                        15  0                                                          
     DPvalor                         14  2                                                          
     DPcampa                          4                                                             
                                                                                                    
     DPgmDia           Pr                  Extpgm('VOTREP00/VTMGOBNR')                              
     DUfecha                          8                                                             
     DUdia                           20                                                             
      *Pgm recupera campaña para recuperar valor de posteo en cartera                               
     DPgmRecuCÑ        PR                  Extpgm('VOTREP00/VTMGOIQR')                              
     DWADTNDÑ                        30                                                             
                                                                                                    
     DPrVtwpgif        Pr                                                                           
                                                                                                    
     ** Parametros de entrada                                                                       
     DMain             Pr                  Extpgm('Main')                                           
     DPcia                            3a                                                            
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
     DPcia                            3a                                                            
                                                                                                    
     DPgmCambiarFechas...                                                                           
     D                 Pr                  Extpgm('VOTREP00/VTUPECOR')                              
     DPcia                            3a                                                            
     DPrVerificarRutGuia...                                                                         
     D                 Pr              n                                                            
                                                                                                    
     DDsNewPlanGu      Ds                                                                           
     Dwcaso                           7a   Inz(*Blanks)                                             
     Dwcedula                        30a   Inz(*Blanks)                                             
     Dwfecha                         10a   Inz(*Blanks)                                             
     DOrespuestaw                     1a   Inz(*Blanks)                                             
                                                                                                    
     DPgmNewPlanG      Pr                  Extpgm('VOTREP00/VTMPGB0R')                              
     DPcia                            3a                                                            
     DIzona                           3a                                                            
     DIcaso                           7a                                                            
     DIcedula                        30a                                                            
     DIfecha                         10a                                                            
     DOrespuesta                      1a                                                            
                                                                                                    
     DPrVerificaSiHayPagosenEstadoE...                                                              
     D                 Pr                                                                           
     DPrPagosPendientesGuias...                                                                     
     D                 Pr                                                                           
     DPgmPoliticasRUT  Pr                  Extpgm('VOTREP00/VTMPGB3R')                              
     DPcia                            3a                                                            
     DIzona                           3a                                                            
     DIcedula                        30a                                                            
     DOrespuestaPgoRUT...                                                                           
     D                                1a                                                            
     DOrespuestaIngCnt...                                                                           
     D                                1a                                                            
     DPrValidarPagos   Pr                                                                           
     DPgmNroCamRut     Pr                  Extpgm('VOTREP00/VTMPGBJR')                              
     DPcia                            3a                                                            
     DIregion                        30a                                                            
     DIzona                           3a                                                            
     DIsector                         3a                                                            
     DIcampa                          4a                                                            
     DInroC                           4  0                                                          
     DPrRegion         Pr            12a                                                            
     DPgmNombreComerciales...                                                                       
     D                 Pr                  Extpgm('VOTREP00/VTPLDITR')                              
     DPcia                            3                                                             
     DHrol                            2a                                                            
     DHfec                           10a                                                            
     DHtexto                        200a                                                            
     DPrVtmipgf        Pr              n                                                            
     DHcedula                        30a                                                            
     DPrValidarCampa   Pr              n                                                            
                                                                                                    
     DPgmValidarPagos...                                                                            
     D                 Pr                  Extpgm('VOTREP00/VTVPGB14R')                             
                                                                                                    
       //------------------------------------------------------------------//                       
       //              P r o g r a m a   P r i n c i p a l                 //                       
       //------------------------------------------------------------------//                       
      /Free                                                                                         
       Hrol = 'GS';                                                                                 
       Htexto = *Blanks;                                                                            
       PgmNombreComerciales(Pcia:Hrol:Hfec:Htexto);                                                 
       Hrol = 'CC';                                                                                 
       HtextoC = *Blanks;                                                                           
       PgmNombreComerciales(Pcia:Hrol:Hfec:HtextoC);                                                
                                                                                                    
       PrValidarPagos();                                                                            
       PrVerificaSiHayPagosenEstadoE();                                                             
       WPrime = 'S';                                                                                
       PCamE = PrCampaÑa();                                                                         
       PCamEE = PCamE;                                                                              
       PCamR = *Blanks;                                                                             
       POper = '-';                                                                                 
       Pnume = '18';                                                                                
       ProCampa(Pcia:PCamE:PCamR:POper:Pnume);                                                      
       PCamR1 = PCamR;                                                                              
                                                                                                    
       If PrDtaara() = *On;                                                                         
         qfecha = %Char((Fecha):*Usa/);                                                             
         Tfecha = %Subst(qfecha:4:2) + '/' + %Subst(qfecha:1:2) + '/' +                             
                %Subst(qfecha:7:4);                                                                 
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
        Endif;                                                                                      
        PgmValidarPagos();                                                                          
        *Inlr = *On;                                                                                
      /End-Free                                                                                     
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Llena el Sub Archivo del Sub File              //                       
       //------------------------------------------------------------------//                       
     PLlenarSfl        B                                                                            
     DLlenarSfl        Pi                                                                           
      /Free                                                                                         
       BorraSfl();                                                                                  
       If WPrime <> 'S';                                                                            
         PrActFechas();                                                                             
          If PPO = 1 Or PPO = 2;                                                                    
            Wsql = 'Select Distinct Spgcia, Spgzon, Spgcam, Spgfds, Spginp, ' +                     
                 'Sum(Spgvps) As Total ' +                                                          
                 'From Vtmspgf ' +                                                                  
                 'Where Spgnbt =' + $COMA + ' ' + $COMA +                                           
                 ' And Spgcia = ' + $COMA + %Trim(Pcia) + $COMA + ' And ' +                         
                 'Spgfnp <> ' + $coma + Tfecha + $coma + ' And ' +                                  
                 'Spgcam >= ' + %Trim(PcamR1) + ' And ' +                                           
                 'Spgvps <> ' + $COMA + '0' + $COMA +                                               
                 ' And Spginp In ('+ $COMA + ' ' + $COMA + ', ' +                                   
                  $COMA + 'T' + $COMA + ', '  +                                                     
                  $COMA + 'I' + $COMA + ', '  +                                                     
                  $COMA + 'R' + $COMA + ') ';                                                       
                                                                                                    
            //Condición para el filtro                                                              
            If WFiltroEst <> ' ';                                                                   
              Wsql = %Trim(Wsql) + ' ' +                                                            
                     'And Spginp in ' + %trim(WFiltroEst);                                          
              Filtrar = *Off;                                                                       
            Endif;                                                                                  
                                                                                                    
                                                                                                    
            Wsql =  %trim ( Wsql ) + ' ' +                                                          
                   ' Group By Spgcia, Spgzon, Spgcam, Spgfds, Spginp ' +                            
                   'Order By Spgcia, Spgcam, Spgzon, Spgfds, Spginp';                               
          ElseIf PPO = 5;                                                                           
            Wsql = 'Select Distinct Spgcia, Spgzon, Spgcam, Spgfds, Spginp, ' +                     
                 'Sum(Spgvps) As Total ' +                                                          
                 'From Vtmspgf ' +                                                                  
                 'Where Spgnbt =' + $COMA + ' ' + $COMA +                                           
         ' And Spgcia = ' + $COMA + %Trim(Pcia) + $COMA + ' And ' +                                 
                 'Spgfnp <> ' + $coma + Tfecha + $coma + ' And ' +                                  
                 'Spgcam >= ' + %Trim(PcamR1) + ' And ' +                                           
                 'Spgvps <> ' + $COMA + '0' + $COMA +                                               
                 ' And Spginp In ('+ $COMA + ' ' + $COMA + ', ' +                                   
                  $COMA + 'T' + $COMA + ', '  +                                                     
                  $COMA + 'I' + $COMA + ', '  +                                                     
                  $COMA + 'R' + $COMA + ') ' +                                                      
                  ' Group By Spgcia, Spgzon, Spgcam, Spgfds, Spginp ' ;                             
                                                                                                    
            Wsql =  %trim ( Wsql ) + ' Union ' +                                                    
           'Select Distinct SpgCia, SpgZon, SpgCam, Char(Date(Pgefec), Usa), ' +                    
                 'Pgeest, Sum(Pgevlp) As Total From Vtmspgf ' +                                     
                 'Inner Join Votrea00/Vtwpgef On '+                                                 
                 'Pgecia = Spgcia And Pgecdg =Spgced  ' +                                           
                 'Where Spgnbt = ' + $COMA + ' ' + $COMA +                                          
                 ' And Spgcia = ' + $COMA + %Trim(Pcia) + $COMA + ' And ' +                         
                 'Spgfnp <> ' + $coma + Tfecha + $coma + ' And ' +                                  
                 'Spgcam >= ' + %Trim(PcamR) +                                                      
                 ' Group By SpgCia, SpgZon, SpgCam, Pgefec, Pgeest' ;                               
                                                                                                    
          Else;                                                                                     
            Wsql = 'Select Distinct SpgCia, SpgZon, SpgCam, Pgefec, Pgeest, ' +                     
                 'Sum(Pgevlp) As Total From Vtmspgf ' +                                             
                 'Inner Join Votrea00/Vtwpgef On '+                                                 
                 'Pgecia = Spgcia And Pgecdg =Spgced ' +                                            
                 'Where Spgnbt = ' + $COMA + ' ' + $COMA +                                          
                 ' And Spgcia = ' + $COMA + %Trim(Pcia) + $COMA + ' And ' +                         
                 'Spgfnp <> ' + $coma + Tfecha + $coma + ' And ' +                                  
                 'Spgcam >= ' + %Trim(PcamR);                                                       
                                                                                                    
                                                                                                    
                  //Condición para el filtro                                                        
            If WFiltroEst <> ' ';                                                                   
              Wsql = %Trim(Wsql) + ' ' +                                                            
                     'And Pgeest in ' + %trim(WFiltroEst);                                          
              Filtrar = *Off;                                                                       
            Endif;                                                                                  
                                                                                                    
            Wsql =  %trim ( Wsql ) + ' ' +                                                          
                    'Group By SpgCia, SpgZon, SpgCam, Pgefec, Pgeest Order ' +                      
                    'By SpgCia, SpgZon, SpgCam, Pgefec, Pgeest';                                    
                                                                                                    
          EndIf;                                                                                    
                                                                                                    
          Exec Sql                                                                                  
            Prepare C6 From :Wsql;                                                                  
                                                                                                    
          Exec Sql                                                                                  
            Declare C1 Scroll Cursor For C6;                                                        
                                                                                                    
          Exec Sql                                                                                  
            Open C1;                                                                                
                                                                                                    
          Exec Sql                                                                                  
            fetch C1 Into :Fields;                                                                  
                                                                                                    
       EndIf;                                                                                       
                                                                                                    
        Dow Sqlcod <> 100 And Sqlcod >= *Zeros;                                                     
         PrValidarLiderActiva();                                                                    
         If QSpgcam >= '1615';                                                                      
         Orespuesta = *Blanks;                                                                      
         Pccte = *Blanks;                                                                           
         Icaso = *Blanks;                                                                           
         Ifecha = *Blanks;                                                                          
         PgmNewPlanG(QSpgcia:QSpgzon:Icaso:Pccte:Ifecha:Orespuesta);                                
                                                                                                    
         If Orespuesta = 'S';                                                                       
         If Total > *Zeros;                                                                         
          *In60 = *Off;                                                                             
          PrPremisa();                                                                              
          If QNumero > *Zeros;                                                                      
           *In60 = *On;                                                                             
          Endif;                                                                                    
          Pop = *Blanks;                                                                            
          Pcampa = QSpgcam;                                                                         
          Pzona =  QSpgzon;                                                                         
          If QSpginp <> 'T';                                                                        
            Pvalor = Total;                                                                         
          Else;                                                                                     
            Pvalor = *Zeros;                                                                        
          EndIf;                                                                                    
          Pfechaf = QSpgfds;                                                                        
          If QSpginp = ' ';                                                                         
           Ptipo = 'Ajuste Crédito';                                                                
          Endif;                                                                                    
          If QSpginp = 'I';                                                                         
           Ptipo = 'Inconsis Pago';                                                                 
          Endif;                                                                                    
          If QSpginp = 'T';                                                                         
           Ptipo = 'P en Efectivo';                                                                 
          Endif;                                                                                    
          If QSpginp = 'R';                                                                         
           Ptipo = 'Pago Realizado';                                                                
          Endif;                                                                                    
          If QSpginp <> ' ' And QSpginp <> 'T' And QSpginp <> 'I'                                   
             And QSpginp <> 'R';                                                                    
           Ptipo = *Blanks;                                                                         
          Endif;                                                                                    
          PSPGINP = QSpginp;                                                                        
          Nrr += 1;                                                                                 
          Posi = Nrr;                                                                               
          Write Rdatos;                                                                             
         Endif;                                                                                     
        Endif;                                                                                      
       Endif;                                                                                       
       Exec Sql                                                                                     
        Fetch C1 Into :Fields;                                                                      
        Enddo;                                                                                      
                                                                                                    
       Exec Sql                                                                                     
        Close C1;                                                                                   
                                                                                                    
        SflEnd = *On;                                                                               
        If Nrr > *Zeros;                                                                            
           Select;                                                                                  
             When PPO   = 1 Or PPO   = 4;                                                           
               SflDsp = *On;                                                                        
               *In39 = *On;                                                                         
               *In41 = *Off;                                                                        
                                                                                                    
             When PPO   = 2;                                                                        
               SflDsp = *On;                                                                        
               *In39 = *Off;                                                                        
               *In41 = *On;                                                                         
                                                                                                    
             When PPO   = 3;                                                                        
               SflDsp = *On;                                                                        
               *In39 = *On;                                                                         
               *In41 = *Off;                                                                        
                                                                                                    
             When PPO   = 5 Or PPO   = 6;                                                           
               SflDsp = *On;                                                                        
               *In39 = *On;                                                                         
               *In41 = *On;                                                                         
           Endsl;                                                                                   
        Else;                                                                                       
         SflDsp = *Off;                                                                             
         *In39 = *Off;                                                                              
         *In41 = *Off;                                                                              
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
       Dow *In03 = *Off;                                                                            
       LlenarSfl();                                                                                 
       If Posix > *Zeros;                                                                           
        Posic = Posix;                                                                              
       Endif;                                                                                       
       Ppais = Xpais;                                                                               
       SflDspCtl = *On;                                                                             
       Write Rteclas;                                                                               
       If WPrime = 'S';                                                                             
         Write Rcontrol;                                                                            
         Filtro();                                                                                  
         WPrime = 'N';                                                                              
       Else;                                                                                        
         Exfmt Rcontrol;                                                                            
       EndIf;                                                                                       
       SflDspCtl = *Off;                                                                            
                                                                                                    
       //Realiza filtros de la información                                                          
       If Filtrar = *On;                                                                            
         Filtro();                                                                                  
         //Iter;                                                                                    
       Endif;                                                                                       
                                                                                                    
       If Posteo = *On And Salir = *Off and *In39 = *On;                                            
        GenerarposteoCartera();                                                                     
       Endif;                                                                                       
                                                                                                    
       //Envia Pagos al Banco                                                                       
       If EnviarPag = *On and *In41 = *On;                                                          
         Enviar();                                                                                  
         Iter;                                                                                      
       Endif;                                                                                       
                                                                                                    
       //Cambiar Fechas de Pago                                                                     
       If CambiarFechas = *On;                                                                      
        PgmCambiarFechas(Pcia);                                                                     
        Iter;                                                                                       
       Endif;                                                                                       
                                                                                                    
       If Salir = *Off And Nrr > *Zeros;                                                            
        Readc Rdatos;                                                                               
         Dow Not %Eof();                                                                            
          Select;                                                                                   
           When Pop = '2';                                                                          
           If Ptipo <> 'P en Efectivo';                                                             
             PintarSflD();                                                                          
             SflDsp = *On;                                                                          
             *In42 = *On;                                                                           
           Else;                                                                                    
             SflDsp = *Off;                                                                         
             *In42 = *Off;                                                                          
             PintarSflDE();                                                                         
           EndIf;                                                                                   
           When Pop = '3';                                                                          
             GenerarposteoCartera();                                                                
           When Pop = '5';                                                                          
            Consultar();                                                                            
          Endsl;                                                                                    
          Pop = *Blanks;                                                                            
          Update Rdatos;                                                                            
         Readc Rdatos;                                                                              
        Enddo;                                                                                      
        Endif;                                                                                      
       Enddo;                                                                                       
      /End-Free                                                                                     
     PPintarSfl        E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Llena el Sub Archivo del Sub File              //                       
       //------------------------------------------------------------------//                       
     PLlenarSflD       B                                                                            
     DLlenarSflD       Pi                                                                           
      /Free                                                                                         
       BorraSfld();                                                                                 
       Exec Sql                                                                                     
        Declare C2 Scroll Cursor For                                                                
        Select Spgcia, Spgsec, Spgtrr, Spgzon, Spgcam, Spgvps, Spgfbt,                              
               Spgced, Spginp                                                                       
        From Vtmspgf                                                                                
        Where Spgnbt = ' ' And Spgcia = :Pcia And Spgfnp <> :Tfecha                                 
              And Spgcam = :Pcampa And Spgzon = :W_ZonaAux                                          
          //  And Spgcam >= :PcamR And                                                              
          And Spginp In (' ', 'I')                                                                  
        Order By Spgcia, Spgcam, Spgzon, Spgsec, Spgtrr, Spgfbt;                                    
                                                                                                    
       Exec Sql                                                                                     
        Open C2;                                                                                    
                                                                                                    
       Exec Sql                                                                                     
        Fetch C2 Into :Fields1;                                                                     
                                                                                                    
        Pcampa1 = RSpgcam;                                                                          
        RTotales = *Zeros;                                                                          
        Dow Sqlcod <> 100 And Sqlcod >= *Zeros;                                                     
          *In64 = *Off;                                                                             
          If RSpgvps > *Zeros;                                                                      
           *In60 = *Off;                                                                            
           If RSpginp <> *Blanks And RSpginp <> 'T';                                                
            *In60 = *On;                                                                            
           Endif;                                                                                   
           Pop = *Blanks;                                                                           
           Pzona = RSpgzon;                                                                         
           Pst1 = RSpgsec;                                                                          
           Pcedula1 = RSpgced;                                                                      
           Pnombre1 = PrNombreGuia();                                                               
           Pvalor1 = RSpgvps;                                                                       
           RTotales = RTotales + Pvalor1;                                                           
           Ptr1 = RSpgtrr;                                                                          
           Nrr1 += 1;                                                                               
           Posi = Nrr1;                                                                             
           Write Rdatos1;                                                                           
          Endif;                                                                                    
       Exec Sql                                                                                     
        Fetch C2 Into :Fields1;                                                                     
        Enddo;                                                                                      
        ZonaAux = Pzona;                                                                            
        If Nrr1 > *Zeros;                                                                           
         *In62 = *On;                                                                               
         *In64 = *On;                                                                               
          Pop = *Blanks;                                                                            
          Pzona = *Blanks;                                                                          
          Pst1 = *Blanks;                                                                           
          Pcedula1 = *Zeros;                                                                        
          Pnombre1 = 'Total...............';                                                        
          Pvalor1 = RTotales;                                                                       
          Ptr1 = *Blanks;                                                                           
          Nrr1 += 1;                                                                                
          Write Rdatos1;                                                                            
          *In62 = *Off;                                                                             
          *In64 = *Off;                                                                             
        Endif;                                                                                      
                                                                                                    
       Exec Sql                                                                                     
        Close C2;                                                                                   
                                                                                                    
        SflEnd = *On;                                                                               
        If Nrr1 > *Zeros;                                                                           
         SflDsp = *On;                                                                              
        Else;                                                                                       
         SflDsp = *Off;                                                                             
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
       Nrr1 = *Zeros;                                                                               
       SflClr = *On;                                                                                
       Write Rcontrol1;                                                                             
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
        If Pzona <> *Blanks;                                                                        
          W_ZonaAux = Pzona;                                                                        
        EndIf;                                                                                      
        LlenarSfld();                                                                               
        If Posix > *Zeros;                                                                          
         Posic = Posix;                                                                             
        Endif;                                                                                      
        Ppais = Xpais;                                                                              
        SflDspCtl = *On;                                                                            
        Write Rteclas;                                                                              
        Exfmt Rcontrol1;                                                                            
        SflDspCtl = *Off;                                                                           
        If Posteo = *On And Salir = *Off and *In39 = *On;                                           
         GenerarposteoCartera();                                                                    
        Endif;                                                                                      
                                                                                                    
                                                                                                    
        //Envia Pagos al Banco                                                                      
        If EnviarPag = *On and *In41 = *On;                                                         
          Pzona = ZonaAux;                                                                          
          If EnviarPag = *On;                                                                       
            Pop = '3';                                                                              
          EndIf;                                                                                    
          Enviar1();                                                                                
          Iter;                                                                                     
        Endif;                                                                                      
                                                                                                    
                                                                                                    
        //Realiza filtros de la información                                                         
          If Filtrar = *On;                                                                         
            Filtro();                                                                               
            Leave;                                                                                  
          Endif;                                                                                    
                                                                                                    
        If Salir = *Off And Nrr1 > *Zeros;                                                          
        Readc Rdatos1;                                                                              
         Dow Not %Eof();                                                                            
          Select;                                                                                   
           When Pop = '4';                                                                          
            BorraGuidelPosteo();                                                                    
          Endsl;                                                                                    
          Pop = *Blanks;                                                                            
          Update Rdatos1;                                                                           
         Readc Rdatos1;                                                                             
        Enddo;                                                                                      
        Endif;                                                                                      
       Enddo;                                                                                       
       If Salir = *On;                                                                              
        Salir = *Off;                                                                               
       Endif;                                                                                       
      /End-Free                                                                                     
     PPintarSflD       E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Busca el Nombre de la Guía                     //                       
       //------------------------------------------------------------------//                       
     PPrNombreGuia     B                                                                            
     DPrNombreGuia     Pi            30                                                             
      /Free                                                                                         
       SqlStr = 'Select Mlname ' +                                                                  
                'From Opf' + Pcia + '/Lmllst01 ' +                                                  
                'Where MlcstÑ = ' + %Trim(%Editc(RSpgced:'Z'));                                     
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur From :SqlStr;                                                                   
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor Cursor For Cur;                                                              
                                                                                                    
       Exec Sql                                                                                     
        Open Cursor;                                                                                
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor Into :Wnombre;                                                                 
                                                                                                    
       If Sqlcod = 100;                                                                             
        Wnombre = *Blanks;                                                                          
       Endif;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close Cursor;                                                                               
                                                                                                    
       Return Wnombre;                                                                              
      /End-Free                                                                                     
     PPrNombreGuia     E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Borra Guía del Posteo                          //                       
       //------------------------------------------------------------------//                       
     PBorraGuidelPosteo...                                                                          
     P                 B                                                                            
     DBorraGuidelPosteo...                                                                          
     D                 Pi                                                                           
      /Free                                                                                         
       Clear Rdato3;                                                                                
       Dow Popc = *Blanks;                                                                          
        Exfmt Rdato3;                                                                               
        If Popc = 'S' Or Popc = 's';                                                                
         Exec Sql                                                                                   
         Update Vtmspgf Set Spginp = 'S', Spgfnp = :Tfecha, Spgvps = 0                              
         Where Spgcia = :Pcia And                                                                   
               Spgzon = :Pzona And                                                                  
               Spgcam = :Pcampa1 And                                                                
               Spgsec = :Pst1 And                                                                   
               Spgtrr = :Ptr1 And                                                                   
               Spgced = :Pcedula1;                                                                  
         *In39 = *On;                                                                               
         Exfmt Rdato3;                                                                              
         *In39 = *Off;                                                                              
         Posix = 1;                                                                                 
        Else;                                                                                       
         Posix = Posi;                                                                              
        Endif;                                                                                      
       Enddo;                                                                                       
      /End-Free                                                                                     
     PBorraGuidelPosteo...                                                                          
     P                 E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Genera Posteo Automatico de Notas Debito en la //                       
       // Cartera a Favor de las Guías                                     //                       
       //------------------------------------------------------------------//                       
     PGenerarposteoCartera...                                                                       
     P                 B                                                                            
     DGenerarposteoCartera...                                                                       
     D                 Pi                                                                           
      /Free                                                                                         
       If Nrr > *Zeros;                                                                             
       Clear Rdato4;                                                                                
       Dow Popc = *Blanks;                                                                          
        Exfmt Rdato4;                                                                               
        If Popc = 'S' Or Popc = 's';                                                                
         AfectarCartera();                                                                          
         //PrFTP();                                                                                 
         *In36 = *On;                                                                               
         Exfmt Rdato4;                                                                              
         *In36 = *Off;                                                                              
        Endif;                                                                                      
       Enddo;                                                                                       
       Endif;                                                                                       
      /End-Free                                                                                     
     PGenerarposteoCartera...                                                                       
     P                 E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Verifica si la Zona Tiene Guias Eliminadas del //                       
       // Posteo                                                           //                       
       //------------------------------------------------------------------//                       
     PPrPremisa        B                                                                            
     DPrPremisa        Pi                                                                           
      /Free                                                                                         
       Exec Sql                                                                                     
        Select Count(Spginp) Into :QNumero                                                          
        From Vtmspgf                                                                                
        Where Spgnbt = ' ' And Spgcia = :Pcia And Spgfnp <> :Tfecha And                             
        Spgfnp <> ' ' And Spgzon = :QSpgzon And Spgcam = :QSpgcam;                                  
      /End-Free                                                                                     
     PPrPremisa        E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Graba Novedades para Afectar la Cartera        //                       
       //------------------------------------------------------------------//                       
     PAfectarCartera   B                                                                            
     DAfectarCartera   Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DTsw              s              1a   Inz(*Blanks)                                             
     DWsig             s              2a   Inz(*Blanks)                                             
     DWano             s              2a   Inz(*Blanks)                                             
     DWaÑo             s              4a   Inz(*Blanks)                                             
     DWmes             s              2a   Inz(*Blanks)                                             
     DWdia             s              2a   Inz(*Blanks)                                             
     DKban             s              3a   Inz(*Blanks)                                             
     DKced             s             15s 0 Inz(*Zeros)                                              
     DKtip             s              2a   Inz(*Blanks)                                             
     DCmd              s           3000a                                                            
     DLen              s             15p 5                                                          
     DWvari            s            200a   Inz(*Blanks)                                             
     Didx              s              1S 0 Inz(*Zeros)                                              
                                                                                                    
     ** Definiciòn de Prototipos de Trabajo                                                         
     DQcmdexc          Pr                  Extpgm('QCMDEXC')                                        
     D                             3000a   Const Options(*Varsize)                                  
     D                               15p 5 Const                                                    
     D                                3a   Const Options(*Nopass)                                   
      /Free                                                                                         
       W_IndFtp = 'N';                                                                              
       Evalr Wsig = %Char(Sig_sys);                                                                 
       Wsig = %Xlate(' ':'0':Wsig);                                                                 
       Evalr Wano = %Char(AÑo_sys);                                                                 
       Wano = %Xlate(' ':'0':Wano);                                                                 
       WaÑo = Wsig + Wano;                                                                          
       Evalr Wmes = %Char(Mes_sys);                                                                 
       Wmes = %Xlate(' ':'0':Wmes);                                                                 
       Evalr Wdia = %Char(Dia_sys);                                                                 
       Wdia = %Xlate(' ':'0':Wdia);                                                                 
                                                                                                    
       If Posteo = *On And Pop = *Blanks;                                                           
         If Ptipo = 'P en Efectivo';                                                                
           SqlStr = 'Select * ' +                                                                   
                   'From Votrea00/Vtmspgf ' +                                                       
                   'Where Spgcia = ' + '''' + Pcia + '''' + ' And ' +                               
                         'Spgnbt = ' + '''' + Blan + '''' + ' And ' +                               
                         '((Spgfnp <> ' + '''' + Tfecha + '''' + ') Or ' +                          
                         '(Spgfnp = ' + '''' + Blan + '''' + ')) And ' +                            
                         'Or Spginp = ''T''' +                                                      
                         ' And ' +                                                                  
                         'Spgvps <> 0 And Spgcam >=' + '''' + PcamR + '''';                         
         Else;                                                                                      
           SqlStr = 'Select * ' +                                                                   
                   'From Votrea00/Vtmspgf ' +                                                       
                   'Where Spgcia = ' + '''' + Pcia + '''' + ' And ' +                               
                         'Spgnbt = ' + '''' + Blan + '''' + ' And ' +                               
                         '((Spgfnp <> ' + '''' + Tfecha + '''' + ') Or ' +                          
                         '(Spgfnp = ' + '''' + Blan + '''' + ')) And ' +                            
                         '(Spginp = '' ''' + ' Or Spginp = ''I''' +                                 
                         ') And ' +                                                                 
                         'Spgvps <> 0 And Spgcam >=' + '''' + PcamR + '''';                         
         EndIf;                                                                                     
       Endif;                                                                                       
                                                                                                    
       If Posteo = *Off And Pop = '3';                                                              
         If Ptipo = 'P en Efectivo';                                                                
           SqlStr = 'Select * ' +                                                                   
                    'From Votrea00/Vtmspgf ' +                                                      
                   'Where Spgcia = ' + '''' + Pcia + '''' + ' And ' +                               
                        'Spgnbt = ' + '''' + Blan + '''' + ' And ' +                                
                        '((Spgfnp <> ' + '''' + Tfecha + '''' + ') Or ' +                           
                        '(Spgfnp = ' + '''' + Blan + '''' + ')) And ' +                             
                        'Spgzon = ' + '''' + Pzona + '''' + ' And ' +                               
                        'Spginp = ''T''' +                                                          
                        ' And ' +                                                                   
                        'Spgvps <> 0 And Spgcam =' + '''' + Pcampa + '''';                          
                                                                                                    
         Else;                                                                                      
           SqlStr = 'Select * ' +                                                                   
                    'From Votrea00/Vtmspgf ' +                                                      
                   'Where Spgcia = ' + '''' + Pcia + '''' + ' And ' +                               
                        'Spgnbt = ' + '''' + Blan + '''' + ' And ' +                                
                        '((Spgfnp <> ' + '''' + Tfecha + '''' + ') Or ' +                           
                        '(Spgfnp = ' + '''' + Blan + '''' + ')) And ' +                             
                        'Spgzon = ' + '''' + Pzona + '''' + ' And ' +                               
                        '(Spginp = '' ''' + ' Or Spginp = ''I''' +                                  
                        ') And ' +                                                                  
                        'Spgvps <> 0 And Spgcam =' + '''' + Pcampa + '''';                          
         EndIf;                                                                                     
       Endif;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur1 From :SqlStr;                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor1 Cursor For Cur1;                                                            
                                                                                                    
       Exec Sql                                                                                     
        Open Cursor1;                                                                               
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor1 Into :Fields2;                                                                
                                                                                                    
        If Sqlcod <> 100 And Sqlcod >= *Zeros;                                                      
         Conse = *Zeros;                                                                            
         Consea = *Blanks;                                                                          
         Conseb = *Blanks;                                                                          
         PrConsecutivo();                                                                           
         Evalr Consea = %Char(Conse);                                                               
         Consea = %Xlate(' ':'0':Consea);                                                           
         Conseb = %Subst(Consea:2:5);                                                               
        Endif;                                                                                      
        Dow Sqlcod <> 100 And Sqlcod >= *Zeros;                                                     
         SpgcedAux = xSpgced;                                                                       
         Clear Tced;                                                                                
         Tced = %Trim(%Editc(xSpgced:'Z'));                                                         
         If PrVerificarRutGuia() = *Off;                                                            
         If xSpginp = *Blanks Or xSpginp = 'I' Or xSpginp = 'i';                                    
           Ycia = Pcia;                                                                             
           Tced = %Trim(%Editc(xSpgced:'Z'));                                                       
           Tcns = *Blanks;                                                                          
           Conversion(Ycia:Tced:Tcns);                                                              
           Icedula = *Zeros;                                                                        
           If %Len(%Trim(Tcns)) > *Zeros;                                                           
             Icedula = %Int(Tcns);                                                                  
           Else;                                                                                    
             Icedula = xSpgced;                                                                     
           Endif;                                                                                   
           Tsw = 'S';                                                                               
           Kban = *Blanks;                                                                          
           Kced = xSpgced;                                                                          
           Kord = *Zeros;                                                                           
           PrBuscarOrden();                                                                         
           Ktip = 'A-';                                                                             
           If %Open(Vtwcabf2) = '0';                                                                
             Open Vtwcabf2;                                                                         
           Endif;                                                                                   
           Chain (Kban:Icedula:Kord:Ktip) Vtwcabf2;                                                 
           If Not %Found(Vtwcabf2);                                                                 
             Wcbced = xSpgced;                                                                      
             Wcbtip = Ktip;                                                                         
             Wcbval = xSpgvps * (-1);                                                               
             Wcbord = Kord;                                                                         
             Wcbbco = *Blanks;                                                                      
             Wcbcmn = 'DctoCdLP' + %Trim(PrCampaÑa1()) + 'mt' +                                     
                       %Trim(xSpgcam);                                                              
             Wcbfet = WaÑo + '/' + Wmes + '/' + Wdia;                                               
             Wcbsts = *Blanks;                                                                      
             Wcbusr = User;                                                                         
             Wcbcct = xSpgccr;                                                                      
             If Wcbcct = *Zeros;                                                                    
               Wcbcct = 042;                                                                        
             Endif;                                                                                 
             PrCerrarPost();                                                                        
             PrActualizar();                                                                        
                                                                                                    
             PagoG = *Blanks;                                                                       
             PagoG = %Trim(%Editc(xSpgced:'Z')) + '-' +                                             
                     %Trim(%Editc(Wcbval:'K')) + 'I';                                               
                                                                                                    
             Write Rwcab2;                                                                          
             PrPremiosEspeciales();                                                                 
             PrVtwpgif();                                                                           
                                                                                                    
             Clear Fields4;                                                                         
             Qlpgcia = XSpgCia;                                                                     
             Qlpgcmp = XSpgcam;                                                                     
             Qlpgzna = XSPGZon;                                                                     
             Qlpgsec = XSPGSec;                                                                     
             Qlpgcdg = XSpgced;                                                                     
             SpgcedAux = XSpgced;                                                                   
             If Wcbcct = 042;                                                                       
                QLpgVdc = xSpgvds;                                                                  
                QLpgFdc = xSpgfbt;                                                                  
             ElseIf Wcbcct = 043;                                                                   
                Qlpgvde = Spgvds;                                                                   
                QLpgFde = xSpgfbt;                                                                  
             EndIf;                                                                                 
            // Recupera Región se usan Variables Temporales                                         
               w_TempCia=Pcia;                                                                      
               w_TempZon=Spgzoni;                                                                   
               Pcia     = QLPgCia;                                                                  
               Spgzoni  = QLPgZna;                                                                  
               QlpgReg  = PrRegion();                                                               
               Pcia     = w_TempCia;                                                                
               Spgzoni  = w_TempZon;                                                                
            //fin recupera Región                                                                   
             PrRecuperaPagosL();                                                                    
             Qlpgmot= Pprmot;                                                                       
             PrVtwlpgf();                                                                           
                                                                                                    
             If Tsw <> *Blanks;                                                                     
               PrVtwcabf2(Pcia);                                                                    
               Tsw = *blanks;                                                                       
               Wvari = *Blanks;                                                                     
               Wvari = 'SBMJOB CMD(CALL PGM(VOTREP00/VTNCBI0L) PARM(' +                             
               '''' + Pcia + '''' + '))' +                                                          
               ' JOB(NOVED_CART) JOBQ(GENERP00/PRCGUIAS)';                                          
               Cmd = %Trim(Wvari);                                                                  
               Callp(E) Qcmdexc(Cmd:%Size(Cmd));                                                    
               PrActualizarVtedndf();                                                               
                                                                                                    
               Pcedula = Wcbced;                                                                    
               Pvalor = Wcbval;                                                                     
               Pcampa = xSpgcam;                                                                    
               PgmMensajeTexto(PtipoW:PCia:Pcedula:Pvalor:Pcampa);                                  
             Endif;                                                                                 
           Endif;                                                                                   
           If %Open(Vtwcabf2) = '1';                                                                
             Close Vtwcabf2;                                                                        
           Endif;                                                                                   
         ElseIf xSpginp = 'T';                                                                      
          Clear Tced;                                                                               
          Tced = %Trim(%Editc(xSpgced:'Z'));                                                        
           If xSpginp = 'T' And PrVtmipgf(Tced) = *On;                                              
           For idx = 1 to 2;                                                                        
             Ycia = Pcia;                                                                           
             Tced = %Trim(%Editc(xSpgced:'Z'));                                                     
             Tcns = *Blanks;                                                                        
             Conversion(Ycia:Tced:Tcns);                                                            
             Icedula = *Zeros;                                                                      
             If %Len(%Trim(Tcns)) > *Zeros;                                                         
               Icedula = %Int(Tcns);                                                                
             Else;                                                                                  
               Icedula = xSpgced;                                                                   
             Endif;                                                                                 
             Tsw = 'S';                                                                             
             Kban = *Blanks;                                                                        
             Kced = xSpgced;                                                                        
             Kord = *Zeros;                                                                         
             PrBuscarOrden();                                                                       
             If idx = 1;                                                                            
               Ktip = 'A-';                                                                         
               Wcbval = xSpgvps * (-1);                                                             
             Else;                                                                                  
               Ktip = 'A+';                                                                         
               Wcbval = xSpgvps;                                                                    
             EndIf;                                                                                 
             If %Open(Vtwcabf2) = '0';                                                              
               Open Vtwcabf2;                                                                       
             Endif;                                                                                 
             Chain (Kban:Icedula:Kord:Ktip) Vtwcabf2;                                               
             If Not %Found(Vtwcabf2);                                                               
               Wcbced = xSpgced;                                                                    
               Wcbtip = Ktip;                                                                       
               Wcbord = Kord;                                                                       
               Wcbbco = *Blanks;                                                                    
               If %Len(%Trim(Htexto)) <= *Zeros;                                                    
                Wcbcmn = 'DctoCdLE' + %Trim(PrCampaÑa1()) + 'mt' +                                  
                         %Trim(xSpgcam);                                                            
               Else;                                                                                
                Wcbcmn = 'DctoCdLE' +  %Trim(PrCampaÑa1()) + 'mt' +                                 
                         %Trim(xSpgcam);                                                            
               Endif;                                                                               
               Wcbfet = WaÑo + '/' + Wmes + '/' + Wdia;                                             
               Wcbsts = *Blanks;                                                                    
               Wcbusr = User;                                                                       
               Wcbcct = 043;                                                                        
               PrCerrarPost();                                                                      
               PrInfPagEfec();                                                                      
               PrActualizar();                                                                      
                                                                                                    
               PagoG = *Blanks;                                                                     
               PagoG = %Trim(%Editc(Wcbced:'Z')) + '-' +                                            
                       %Trim(%Editc(Wcbval:'K')) + 'I';                                             
                                                                                                    
               Write Rwcab2;                                                                        
               PrPremiosEspeciales();                                                               
               PrVtwpgif();                                                                         
               If Idx = 1;                                                                          
                  Clear Fields4;                                                                    
                  QLpgcia = XSpgcia;                                                                
                  QLpgcmp = XSpgcam;                                                                
                  QLpgzna = XSpgzon;                                                                
                  QLpgsec = XSpgsec;                                                                
                  QLpgcdg = XSpgced;                                                                
                  QLpgvde = XSpgvds;                                                                
                  QLpgfde = xSpgfbt;                                                                
              // Recupera Región se usan Variables Temporales                                       
                  w_TempCia=Pcia;                                                                   
                  w_TempZon=Spgzoni;                                                                
                  Pcia     = QLPgCia;                                                               
                  Spgzoni  = QLPgZna;                                                               
                  QlpgReg  = PrRegion();                                                            
                  Pcia     = w_TempCia;                                                             
                  Spgzoni  = w_TempZon;                                                             
              //fin recupera Región                                                                 
                Qlpgmot= Pprmot;                                                                    
                PrRecuperaPagosL();                                                                 
                PrVtwlpgf();                                                                        
               Endif;                                                                               
             If Tsw <> *Blanks;                                                                     
              PrVtwcabf2(Pcia);                                                                     
              Tsw = *blanks;                                                                        
              Wvari = *Blanks;                                                                      
              Wvari = 'SBMJOB CMD(CALL PGM(VOTREP00/VTNCBI0L) PARM(' +                              
              '''' + Pcia + '''' + '))' +                                                           
              ' JOB(NOVED_CART) JOBQ(GENERP00/PRCGUIAS)';                                           
              Cmd = %Trim(Wvari);                                                                   
              Callp(E) Qcmdexc(Cmd:%Size(Cmd));                                                     
              PrActualizarVtedndf();                                                                
             Endif;                                                                                 
            Endif;                                                                                  
            If %Open(Vtwcabf2) = '1';                                                               
             Close Vtwcabf2;                                                                        
            Endif;                                                                                  
           EndFor;                                                                                  
            Endif;                                                                                  
           W_IndFtp = 'S';                                                                          
         Endif;                                                                                     
        Else;                                                                                       
         If PrVerificarRutGuia() = *On;                                                             
          Exec Sql                                                                                  
           Update Vtmspgf Set Spginp = 'E'                                                          
           Where Spgcia = :xSpgcia And                                                              
                 Spgzon = :xSpgzon And                                                              
                 Spgcam = :xSpgcam And                                                              
                 Spgsec = :xSpgsec And                                                              
                 Spgtrr = :xSpgtrr And                                                              
                 Spgced = :xSpgced;                                                                 
          If SqlCod <> 100 And SqlCod >= *Zeros;                                                    
           PrPagosPendientesGuias();                                                                
          Endif;                                                                                    
         Endif;                                                                                     
        Endif;                                                                                      
       Exec Sql                                                                                     
        Fetch Cursor1 Into :Fields2;                                                                
       Enddo;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close Cursor1;                                                                              
                                                                                                    
       If W_IndFtp = 'S';                                                                           
         PrFTP();                                                                                   
       EndIf;                                                                                       
                                                                                                    
      /End-Free                                                                                     
     PAfectarCartera   E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Busca la Orden de la Compradora en la Campaña Actual y si no la  //                       
       // Encuentra le Asigna la Orden(888888888) por Defecto.             //                       
       //------------------------------------------------------------------//                       
     PPrBuscarOrden    B                                                                            
     DPrBuscarOrden    Pi                                                                           
      /Free                                                                                         
       Ycia = Pcia;                                                                                 
       Tced = %Trim(%Editc(xSpgced:'Z'));                                                           
       Tcns = *Blanks;                                                                              
       Conversion(Ycia:Tced:Tcns);                                                                  
       Icedula = *Zeros;                                                                            
       If %Len(%Trim(Tcns)) > *Zeros;                                                               
        Icedula = %Int(Tcns);                                                                       
       Else;                                                                                        
        Icedula = xSpgced;                                                                          
       Endif;                                                                                       
       Setll Icedula Lorhdr07;                                                                      
       Reade Icedula Lorhdr07;                                                                      
       Dow Not %Eof(Lorhdr07);                                                                      
        If Ohrcst = 'A' And Ohcmfl = 'N';                                                           
         Kord = OhordÑ;                                                                             
         Leave;                                                                                     
        Else;                                                                                       
         Wsald = *Zeros;                                                                            
         PrSaldo();                                                                                 
         If Wsald > *Zeros;                                                                         
          If Ohrcst = 'S' And Ohcmfl = 'N';                                                         
           Kord = OhordÑ;                                                                           
           Leave;                                                                                   
          Endif;                                                                                    
         Endif;                                                                                     
        Endif;                                                                                      
       Reade Icedula Lorhdr07;                                                                      
       Enddo;                                                                                       
       If Kord = *Zeros;                                                                            
        Kord = Wordc;                                                                               
       Endif;                                                                                       
      /End-Free                                                                                     
     PPrBuscarOrden    E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Determina el Saldo en Cartera de la Compradora                   //                       
       //------------------------------------------------------------------//                       
     PPrSaldo          B                                                                            
     DPrSaldo          Pi                                                                           
      /Free                                                                                         
                                                                                                    
       // Aplica para Colombia                                                                      
       If Pcia <= '002';                                                                            
        SqlStr = *Blanks;                                                                           
        SqlStr = 'Select Sum(Adtram) ' +                                                            
                 'From Votrea' + Pcia + '/Vlardtl01 ' +                                             
                 'Where AdcstÑ = ' + %Trim(%Editc(xSpgced:'Z')) +                                   
                  ' Or AdcstÑ = ' + %Trim(%Editc(Icedula:'Z'));                                     
       // Aplica para los Demas Paises                                                              
       ElseIf Pcia > '002';                                                                         
        SqlStr = *Blanks;                                                                           
        SqlStr = 'Select Sum(Adtram) ' +                                                            
                 'From Opf' + Pcia + '/Lardtl01 ' +                                                 
                 'Where AdcstÑ = ' + %Trim(%Editc(xSpgced:'Z')) +                                   
                 ' Or AdcstÑ = ' + %Trim(%Editc(Icedula:'Z'));                                      
       Endif;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur2 From :SqlStr;                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor2 Cursor For Cur2;                                                            
                                                                                                    
       Exec Sql                                                                                     
        Open Cursor2;                                                                               
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor2 Into :Wsald;                                                                  
                                                                                                    
       If Sqlcod = 100;                                                                             
        Wsald = *Zeros;                                                                             
       Endif;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close Cursor2;                                                                              
                                                                                                    
      /End-Free                                                                                     
     PPrSaldo          E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Consulta Consecutivo del Batch a Generar       //                       
       //------------------------------------------------------------------//                       
     PPrConsecutivo    B                                                                            
     DPrConsecutivo    Pi                                                                           
      /Free                                                                                         
       Chain Pcia Vtmcpgf;                                                                          
       If %Found(Vtmcpgf);                                                                          
        Conse = Cpgcns;                                                                             
        Cpgcns += 1;                                                                                
        Update Regcpg;                                                                              
       Endif;                                                                                       
      /End-Free                                                                                     
     PPrConsecutivo    E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Actualiza Archivo Vtmspgf con la Información del Posteo Generado //                       
       //------------------------------------------------------------------//                       
     PPrActualizar     B                                                                            
     DPrActualizar     Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DTfecha           s             10a   Inz(*Blanks)                                             
     DWtexto           s             10a   Inz(*Blanks)                                             
     DFecha            s             10a   Inz(*Blanks)                                             
     DHora             s              8a   Inz(*Blanks)                                             
     DWcod             s              9  0 Inz(0)                                                   
      /Free                                                                                         
       Wtexto = 'PG' + %Trim(xSpgzon) + Conseb;                                                     
       Tfecha = %Subst(qfecha:4:2) + '/' + %Subst(qfecha:1:2) + '/' +                               
                %Subst(qfecha:7:4);                                                                 
       Exec Sql                                                                                     
        Update Vtmspgf Set Spgnbt = :Wtexto, Spgfbt = :Tfecha, Spgvps = 0                           
        Where Spgcia = :xSpgcia And                                                                 
              Spgzon = :xSpgzon And                                                                 
              Spgcam = :xSpgcam And                                                                 
              Spgsec = :xSpgsec And                                                                 
              Spgtrr = :xSpgtrr And                                                                 
              Spgccr = :Wcbcct And                                                                  
              (Spgced = :xSpgced Or                                                                 
              Spgced = :Icedula) And                                                                
              Spgnbt = ' ';                                                                         
                                                                                                    
       Exec Sql                                                                                     
        Update Votrea00/Vtwpacf Set Pacidp = 'S'                                                    
        Where Paccia = :xSpgcia And                                                                 
              (Paccdg = :Icedula Or Paccdg = :xSpgced) And                                          
              Paccmp = :xSpgcam;                                                                    
                                                                                                    
       //Archivo de Trabajo Inscrip o Re-incorpora Rentable                                         
       Exec Sql                                                                                     
        Update Votrea00/Vtwiegf Set Iegtpa = 'S'                                                    
        Where Iegcia = :xSpgcia And                                                                 
              (Iegcdg = :Icedula Or Iegcdg = :xSpgced) And                                          
              Iegcmp = :xSpgcam And Iegzng = :xSpgzon And                                           
              Iegscg = :xSpgsec And Iegtpa = 'I';                                                   
                                                                                                    
        If SqlCod = 100 Or SqlCod < 0;                                                              
         Fecha = %Char(%Date());                                                                    
         Hora = %Char(%Time());                                                                     
         Wcod = Sqlcod;                                                                             
        Endif;                                                                                      
                                                                                                    
       //Arch de Trabajo Pagos Guías por Stencil con Pedido                                         
       Exec Sql                                                                                     
        Update Votrea00/Vttdggf Set Dggidp = 'P'                                                    
        Where Dggcia = :xSpgcia And                                                                 
              (Dggcdg = :Icedula Or Dggcdg = :xSpgced) And                                          
              Dggcmp = :xSpgcam And Dggzng = :xSpgzon And                                           
              Dggscg = :xSpgsec And Dggidp = 'I';                                                   
                                                                                                    
        If SqlCod = 100 Or SqlCod < 0;                                                              
         Fecha = %Char(%Date());                                                                    
         Hora = %Char(%Time());                                                                     
         Wcod = Sqlcod;                                                                             
        Endif;                                                                                      
                                                                                                    
                                                                                                    
       //Arch de Trabajo Pagos Guías Foco(Conseguir-Conservar)                                      
       Exec Sql                                                                                     
        Update Votrea00/Vttpgbf Set Pgbidp = 'P'                                                    
        Where Pgbcia = :xSpgcia And                                                                 
              (Pgbcdg = :Icedula Or Pgbcdg = :xSpgced) And                                          
              Pgbcmp = :xSpgcam And Pgbzng = :xSpgzon And                                           
              Pgbscg = :xSpgsec And Pgbidp = 'I';                                                   
                                                                                                    
        If SqlCod = 100 Or SqlCod < 0;                                                              
         Fecha = %Char(%Date());                                                                    
         Hora = %Char(%Time());                                                                     
         Wcod = Sqlcod;                                                                             
        Endif;                                                                                      
                                                                                                    
        PrGrabarCierre();                                                                           
                                                                                                    
      /End-Free                                                                                     
     PPrActualizar     E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Graba Cierre de Zona Calificación Guías        //                       
       //------------------------------------------------------------------//                       
     PPrGrabarCierre   B                                                                            
     DPrGrabarCierre   Pi                                                                           
      /Free                                                                                         
       Chain (xSpgcia:xSpgzon:xSpgcam) Vttgzcf;                                                     
       If Not %Found(Vttgzcf);                                                                      
        Rzccia = xSpgcia;                                                                           
        Rzczon = xSpgzon;                                                                           
        Rzccam = xSpgcam;                                                                           
        Rzcidc = 'S';                                                                               
        Monitor;                                                                                    
         Write Reggzc;                                                                              
        On-Error;                                                                                   
         Chain (xSpgcia:xSpgzon:xSpgcam) Vttgzcf;                                                   
         If %Found(Vttgzcf);                                                                        
          Rzcidc = 'S';                                                                             
          Update Reggzc;                                                                            
         Endif;                                                                                     
        EndMon;                                                                                     
       Endif;                                                                                       
      /End-Free                                                                                     
     PPrGrabarCierre   E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Actualiza Registro en Premios Especiales Dinero//                       
       //------------------------------------------------------------------//                       
     PPrPremiosEspeciales...                                                                        
     P                 B                                                                            
     DPrPremiosEspeciales...                                                                        
     D                 Pi                                                                           
      /Free                                                                                         
       Exec Sql                                                                                     
        Update Vtmpegf Set Pegipg = 'S'                                                             
        Where Pegcia = :xSpgcia And                                                                 
              (Pegceg = :xSpgced Or                                                                 
               Pegceg = :Icedula) And                                                               
               Pegipg = ' ';                                                                        
      /End-Free                                                                                     
     PPrPremiosEspeciales...                                                                        
     P                 E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Graba Cierre de Zona Calificación Guías por    //                       
       // Contacto                                                         //                       
       //------------------------------------------------------------------//                       
     PPrCerrarPost     B                                                                            
     DPrCerrarPost     Pi                                                                           
     Dquser            s             10a   Inz(*Blanks)                                             
     DqFecha           s             10a   Inz(*Blanks)                                             
     DqHora            s              8a   Inz(*Blanks)                                             
      /Free                                                                                         
                                                                                                    
       qUser   = User;                                                                              
       qFecha  = %char(%date():*Iso);                                                               
       qHora   = %char(%time():*Iso);                                                               
                                                                                                    
        Exec Sql                                                                                    
         Update Votrea00/Vtmpegf Set Pegipg = 'S'                                                   
         Where Pegcia = :xSpgcia And                                                                
               (Pegceg = :xSpgced Or                                                                
               Pegceg = :Icedula) And                                                               
               Pegipg = 'I';                                                                        
                                                                                                    
      /End-Free                                                                                     
     PPrCerrarPost     E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Actualiza Fecha Cuando no se Genera el Posteo  //                       
       // el Mismo Día                                                     //                       
       //------------------------------------------------------------------//                       
     PPrActFechas      B                                                                            
     DPrActFechas      Pi                                                                           
      ** Definicion de Variables a Utilizar                                                         
     DFecha            s               d   Datfmt(*Usa) Inz(*Sys)                                   
     DqFecha           s             10a   Inz(*Blanks)                                             
     DoFecha           s             10a   Inz(*Blanks)                                             
      /Free                                                                                         
       //Qfecha = %Char((Fecha):*Usa/);                                                             
       //Ofecha = %Subst(qfecha:4:2) + '/' + %Subst(qfecha:1:2) + '/' +                             
       //         %Subst(qfecha:7:4);                                                               
       Qfecha = %Char(%Date());                                                                     
       Exec Sql                                                                                     
        Update Votrea00/Vtmspgf Set Spgfds = :Qfecha                                                
        Where Spgnbt = ' ' And Spgcia = :Pcia And Spgfnp <> :Tfecha                                 
              And Spgvps <> 0;                                                                      
      /End-Free                                                                                     
     PPrActFechas      E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Consulta si este Proceso puede ser Generado    //                       
       //------------------------------------------------------------------//                       
     PPrDtaara         B                                                                            
     DPrDtaara         Pi              n                                                            
      ** Definición de Variables de Trabajo                                                         
     DProcesoW         s               n   Inz(*Off)                                                
     DPantalla         s             10a   Inz(*Blanks)                                             
     DCmd              s            500a   Inz(*Blanks)                                             
      /Free                                                                                         
       In *Lock MyDtaara;                                                                           
        Select;                                                                                     
         When Proceso = ' ';                                                                        
          ProcesoW = *On;                                                                           
         When Proceso <> ' ';                                                                       
          Cmd = 'SNDBRKMSG  MSG(' + '''' + 'La opción de posteo se esta ' +                         
                'ejecutado por otro usuario.. Intente más ' +                                       
                'tarde' + '''' + ') TOMSGQ(' + %Trim(Job) + ')';                                    
          Callp(E) Qcmdexc(Cmd:%Size(Cmd));                                                         
        EndSl;                                                                                      
       Out MyDtaara;                                                                                
       Return ProcesoW;                                                                             
      /End-Free                                                                                     
     PPrDtaara         E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Consulta la Campaña Actual                     //                       
       //------------------------------------------------------------------//                       
     PPrCampaÑa        B                                                                            
     DPrCampaÑa        Pi             4a                                                            
     ** Definiciòn de Variables de Trabajo                                                          
     DSqlStr           s           1000a   Inz(*Blanks)                                             
     DPCampaÑaAct      s              4a   Inz(*Blanks)                                             
      /Free                                                                                         
       SqlStr = *Blanks;                                                                            
       SqlStr = 'Select Trim(Gcmeda) ' +                                                            
                'From Opf' + Pcia + '/Pmtcmp';                                                      
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur6 From :SqlStr;                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor6 Cursor For Cur6;                                                            
                                                                                                    
       Exec Sql                                                                                     
        Open Cursor6;                                                                               
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor6 Into :PCampaÑaAct;                                                            
                                                                                                    
       If Sqlcod = 100 Or Sqlcod < *Zeros;                                                          
        PCampaÑaAct = *Blanks;                                                                      
       Endif;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close Cursor6;                                                                              
                                                                                                    
       Return PCampaÑaAct;                                                                          
      /End-Free                                                                                     
     PPrCampaÑa        E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Consulta la Campaña Actual                     //                       
       //------------------------------------------------------------------//                       
     PPrCampaÑa1       B                                                                            
     DPrCampaÑa1       Pi             4a                                                            
     ** Definiciòn de Variables de Trabajo                                                          
     DSqlStr           s           1000a   Inz(*Blanks)                                             
     DPCampaÑaAct      s              4a   Inz(*Blanks)                                             
      /Free                                                                                         
       SqlStr = *Blanks;                                                                            
       SqlStr = 'Select EstCam ' +                                                                  
                'From Orderp00/Opiestf1 ' +                                                         
                'Where EstCia = ' + $Coma + PCia + $Coma +                                          
                ' And EstZon = ' + $Coma + PZona + $Coma +                                          
                ' And EstEst = ''S''' +                                                             
                ' Order by Rrn(Opiestf1) Desc ' +                                                   
                'Fetch First 1 Row Only';                                                           
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur7 From :SqlStr;                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor7 Cursor For Cur7;                                                            
                                                                                                    
       Exec Sql                                                                                     
        Open Cursor7;                                                                               
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor7 Into :PCampaÑaAct;                                                            
                                                                                                    
       If Sqlcod = 100 Or Sqlcod < *Zeros;                                                          
        PCampaÑaAct = *Blanks;                                                                      
       Endif;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close Cursor7;                                                                              
                                                                                                    
       Return PCampaÑaAct;                                                                          
      /End-Free                                                                                     
     PPrCampaÑa1       E                                                                            
       //------------------------------------------------------------------//                       
       // Procedimiento que Actualiza el estado de Procesado en el Archivo //                       
       // Vtedndf                                                          //                       
       //------------------------------------------------------------------//                       
     PPrActualizarVtedndf...                                                                        
     P                 B                                                                            
     DPrActualizarVtedndf...                                                                        
     D                 Pi                                                                           
     ** Definiciòn de Variables de Trabajo                                                          
     DSqlStr           s           1000a   Inz(*Blanks)                                             
     DOcia             s              3a   Inz(*Blanks)                                             
      /Free                                                                                         
       SqlStr = *Blanks;                                                                            
       Ocia = Pcia;                                                                                 
       If Pcia = '002';                                                                             
        Ocia = '001';                                                                               
       Endif;                                                                                       
       SqlStr = 'Update Votrea' + Ocia + '/Vtedndf Set ' +                                          
                'Dndord = ' + %Trim(%Editc(Kord:'Z')) +                                             
                ', Dndidp = ''S''' + ' Where Dndcia = ' +                                           
                '''' + Pcia + '''' + ' And Dndcde = ' +                                             
                %Trim(%Editc(xSpgced:'Z')) + ' And Dndidp ='' ''';                                  
                                                                                                    
       Exec Sql                                                                                     
        Execute Immediate :SqlStr;                                                                  
                                                                                                    
      /End-Free                                                                                     
     PPrActualizarVtedndf...                                                                        
     P                 E                                                                            
       //------------------------------------------------------------------//                       
       // Procedimiento que cambia los estados registrados en "T" por "P"' //                       
       //------------------------------------------------------------------//                       
     PPrInfPagEfec     B                                                                            
     DPrInfPagEfec     Pi                                                                           
     DNrrAux           S              5i 0 Inz(*Zeros)                                              
     DIdx              S              5i 0 Inz(*Zeros)                                              
     DOcedulaG         S             15  0 Inz(*Zeros)                                              
     DOcedulaGc        S              9  0 Inz(*Zeros)                                              
     DWIpgcgc          S             15  0 Inz(*Zeros)                                              
     DUcuenta          S            100    Inz(' ')                                                 
     DWtipoC           S              2a                                                            
     DWCodigoB         S              3a                                                            
     DWIpgnrc          S            100a                                                            
      /Free                                                                                         
        NrrAux = Nrr;                                                                               
        For idx = 1 to NrrAux;                                                                      
          Chain idx Rdatos;                                                                         
                                                                                                    
          Exec Sql                                                                                  
          Update Votrea00/Vtmspgf                                                                   
            Set SPGINP = 'P'                                                                        
            Where Spgcia = :Pcia And Spgcam >= :PCAMPA                                              
                And Spginp = 'T' And SpgZon = :PZONA                                                
                And Spgvps > 0;                                                                     
                                                                                                    
          If sqlcode = *Zeros;                                                                      
            SetLl (Pcia:PZONA:PCAMPA) Vtmspgf;                                                      
            ReadE (Pcia:PZONA:PCAMPA) Vtmspgf;                                                      
            Dow Not %Eof(Vtmspgf);                                                                  
             If SpgInp = 'P' And Spgvps > 0;                                                        
              Ycia = Pcia;                                                                          
              Tced = %Trim(%Editc(SPGCED:'Z'));                                                     
              Tcns = *Blanks;                                                                       
              Conversion(Ycia:Tced:Tcns);                                                           
              OcedulaG = SPGCED;                                                                    
              OcedulaGc = *Zeros;                                                                   
              OcedulaGc = %Int(Tcns);                                                               
              SpgcedAux = Spgced;                                                                   
              Clear WIpgnrc;                                                                        
              If PrVtmipgf(Tced) = *On;                                                             
               Exec Sql                                                                             
                 Select Ipgcgc, Ipgnrc Into :WIpgcgc, :WIpgnrc                                      
                 From Votrea00/Vtmipgf                                                              
                 Where Ipgcia = :Pcia And                                                           
                      (Trim(Char(Ipgcgc)) = Trim(:Tced) Or                                          
                      Trim(Char(Ipgcgc)) = Trim(:Tcns))                                             
                      And (IpgEst = ' ' Or IpgEst = 'D')                                            
                      Order By RRN(Vtmipgf) Desc                                                    
                      fetch first 1 row Only;                                                       
              Else;                                                                                 
               Exec Sql                                                                             
                 Select Ipgcgc, Ipgnrc Into :WIpgcgc, :WIpgnrc                                      
                 From Votrea00/Vtmipgf                                                              
                 Where Ipgcia = :Pcia And                                                           
                      (Trim(Char(Ipgcgc)) = Trim(:Tced) Or                                          
                      Trim(Char(Ipgcgc)) = Trim(:Tcns))                                             
                      And IpgEst = ' '                                                              
                      Order By RRN(Vtmipgf) Desc                                                    
                      fetch first 1 row Only;                                                       
              Endif;                                                                                
                                                                                                    
              If WIpgcgc <> *Zeros;                                                                 
                OcedulaG = WIpgcgc;                                                                 
              Endif;                                                                                
              Setll (Pcia:OcedulaG) Vtmipgf;                                                        
              ReadE (Pcia:OcedulaG) Vtmipgf;                                                        
              Dow Not %Eof();                                                                       
                If (IPGEST = ' ' Or PrVtmgldf() = *On)                                              
                   And IPGNRC <> *Blanks And IPGNRC = WIpgnrc;                                      
                  PGECIA = Pcia;                                                                    
                  PGECDT = IPGCDC;                                                                  
                  Clear Ucuenta;                                                                    
                  Clear WtipoC;                                                                     
                  Clear WCodigoB;                                                                   
                  Exec Sql                                                                          
                   Select Ipgnrc, Ipgcdc, Ipgcdb                                                    
                   Into :Ucuenta, :WtipoC, :WCodigoB                                                
                   From Votrea00/Vtmipgf                                                            
                   Where Ipgcia = :Pcia And                                                         
                         Ipgcgc = :OcedulaG And                                                     
                         Ipgest = ' ' And                                                           
                         Ipgnrc = :WIpgnrc                                                          
                   Fetch First 1 Row Only;                                                          
                  If Ucuenta <> ' ';                                                                
                   Ipgnrc = Ucuenta;                                                                
                   Pgecdt = WtipoC;                                                                 
                   Pgecdb = WCodigoB;                                                               
                  Endif;                                                                            
                  PGENRC = IPGNRC;                                                                  
                  Chain (Pcia:PGECDT) VTMTPCF;                                                      
                  If %Found(VTMTPCF);                                                               
                    PGEDST = TPCDES;                                                                
                  Else;                                                                             
                    PGEDST = ' ';                                                                   
                  EndIf;                                                                            
                                                                                                    
                  If Pgecdb = ' ';                                                                  
                   PGECDB = IPGCDB;                                                                 
                  Endif;                                                                            
                  Chain (Pcia:PGECDB) VTMBNSF;                                                      
                  If %Found(VTMBNSF);                                                               
                    PGEDSB = BNSDES;                                                                
                  Else;                                                                             
                    PGEDSB = ' ';                                                                   
                  EndIf;                                                                            
                                                                                                    
                  PGEZNG = Pzona;                                                                   
                  PGECMP = Pcampa;                                                                  
                  PGECDG = %Int(Tced);                                                              
                                                                                                    
                   Exec Sql                                                                         
                     Declare C9 Scroll Cursor For                                                   
                     Select SPGVPS                                                                  
                     From Vtmspgf                                                                   
                     Where Spgcia = :Pcia And SPGZON = :PGEZNG                                      
                     And Spgcam = :PGECMP And                                                       
                     (Trim(Char(SPGCED)) = Trim(:Tced) Or                                           
                      Trim(Char(SPGCED)) = Trim(:Tcns))                                             
                     Order By Rrn(Vtmspgf) Desc                                                     
                     Fetch First 1 Row Only;                                                        
                                                                                                    
                     Exec Sql                                                                       
                      Open C9;                                                                      
                                                                                                    
                     Exec Sql                                                                       
                      Fetch C9 Into :W_ValTra;                                                      
                                                                                                    
                     Exec Sql                                                                       
                       Close C9;                                                                    
                  PGEVLP = W_ValTra;                                                                
                                                                                                    
                  Chain (OcedulaGc) LMLLST01;                                                       
                  If %Found(LMLLST01);                                                              
                    PGENMG = MLNAME;                                                                
                  Else;                                                                             
                    PGENMG = ' ';                                                                   
                  EndIf;                                                                            
                  PGEEST = 'P';                                                                     
                                                                                                    
                  PGEFEC = %date();                                                                 
                  PGEHOR = %time();                                                                 
                  PGEUSR =  User;                                                                   
                  PGECPG = PrCampaÑa1();                                                            
                  PGESEC = Spgsec;                                                                  
            If PGECPG = '2009';                                                                     
             PGECPG = '2008';                                                                       
            Endif;                                                                                  
                  If Prvtwpgef() = *Off;                                                            
                   Monitor;                                                                         
                    Write REGPGE;                                                                   
                   On-Error 1021:*FILE;                                                             
                   Endmon;                                                                          
                  Endif;                                                                            
                EndIf;                                                                              
                ReadE (Pcia:OcedulaG) Vtmipgf;                                                      
              EndDo;                                                                                
             EndIf;                                                                                 
             ReadE (Pcia:PZONA:PCAMPA) Vtmspgf;                                                     
            //EndIf;                                                                                
            EndDo;                                                                                  
          EndIf;                                                                                    
          Exec Sql                                                                                  
            Update Votrea00/Vtmspgf                                                                 
              Set SpgVps = 0                                                                        
              Where Spgcia = :Pcia And Spgcam >= :PCAMPA                                            
                  And Spginp = 'P' And SpgZon = :PZONA;                                             
        EndFor;                                                                                     
                                                                                                    
      /End-Free                                                                                     
     PPrInfPagEfec     E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que cambia los estados registrados en "T" por "P"' //                       
       //------------------------------------------------------------------//                       
     PPrInfPagEfec1    B                                                                            
     DPrInfPagEfec1    Pi                                                                           
     DNrrAux           S              5i 0 Inz(*Zeros)                                              
     DIdx              S              5i 0 Inz(*Zeros)                                              
     DOcedulaG         S             15  0 Inz(*Zeros)                                              
     DOcedulaGc        S              9  0 Inz(*Zeros)                                              
     DWIpgcgc          S             15  0 Inz(*Zeros)                                              
     DUcuenta          S            100    Inz(' ')                                                 
      /Free                                                                                         
        NrrAux = Nrr1 - 1;                                                                          
        For idx = 1 to NrrAux;                                                                      
          Chain idx Rdatos1;                                                                        
                                                                                                    
          Exec Sql                                                                                  
          Update Votrea00/Vtmspgf                                                                   
            Set SPGINP = 'P'                                                                        
          Where Spgcia = :Pcia And Spgcam >= :PCAMPA                                                
            And Spginp = 'T' And SpgZon = :PZONA;                                                   
                                                                                                    
          If sqlcode = *Zeros;                                                                      
            SetLl (Pcia:PZONA:PCAMPA) Vtmspgf;                                                      
            ReadE (Pcia:PZONA:PCAMPA) Vtmspgf;                                                      
            //Chain (Pcia:PZONA:PCAMPA) Vtmspgf;                                                    
            //If %Found(Vtmspgf) And SpgInp = 'P';                                                  
            Dow Not %Eof(Vtmspgf);                                                                  
             If SpgInp = 'P';                                                                       
              Ycia = Pcia;                                                                          
              Tced = %Trim(%Editc(SPGCED:'Z'));                                                     
              Tcns = *Blanks;                                                                       
              Conversion(Ycia:Tced:Tcns);                                                           
              OcedulaG = SPGCED;                                                                    
              OcedulaGc = *Zeros;                                                                   
              OcedulaGc = %Int(Tcns);                                                               
              Exec Sql                                                                              
                Select Ipgcgc Into :WIpgcgc                                                         
                From Votrea00/Vtmipgf                                                               
                Where Ipgcia = :Pcia And                                                            
                     (Trim(Char(Ipgcgc)) = Trim(:Tced) Or                                           
                     Trim(Char(Ipgcgc)) = Trim(:Tcns))                                              
                     And IpgEst = ' '                                                               
                     Order By RRN(Vtmipgf) Desc                                                     
                     fetch first 1 row Only;                                                        
              If WIpgcgc <> *Zeros;                                                                 
                OcedulaG = WIpgcgc;                                                                 
              Endif;                                                                                
              Setll (Pcia:OcedulaG) Vtmipgf;                                                        
              ReadE (Pcia:OcedulaG) Vtmipgf;                                                        
              Dow Not %Eof();                                                                       
                If (IPGEST = ' ' Or PrVtmgldf() = *On)                                              
                   And IPGNRC <> *Blanks;                                                           
                  PGECIA = Pcia;                                                                    
                  PGECDT = IPGCDC;                                                                  
                  Clear Ucuenta;                                                                    
                  Exec Sql                                                                          
                   Select Ipgnrc Into :Ucuenta                                                      
                   From Votrea00/Vtmipgf                                                            
                   Where Ipgcia = :Pcia And                                                         
                         Ipgcgc = :OcedulaG And                                                     
                         Ipgest = ' '                                                               
                   Fetch First 1 Row Only;                                                          
                  If Ucuenta <> ' ';                                                                
                   Ipgnrc = Ucuenta;                                                                
                  Endif;                                                                            
                  PGENRC = IPGNRC;                                                                  
                  Chain (Pcia:PGECDT) VTMTPCF;                                                      
                  If %Found(VTMTPCF);                                                               
                    PGEDST = TPCDES;                                                                
                  Else;                                                                             
                    PGEDST = ' ';                                                                   
                  EndIf;                                                                            
                                                                                                    
                  If Pgecdb = ' ';                                                                  
                   PGECDB = IPGCDB;                                                                 
                  Endif;                                                                            
                  Chain (Pcia:PGECDB) VTMBNSF;                                                      
                  If %Found(VTMBNSF);                                                               
                    PGEDSB = BNSDES;                                                                
                  Else;                                                                             
                    PGEDSB = ' ';                                                                   
                  EndIf;                                                                            
                                                                                                    
                  PGEZNG = Pzona;                                                                   
                  PGECMP = Pcampa;                                                                  
                  PGECDG = %Int(Tced);                                                              
                                                                                                    
                   Exec Sql                                                                         
                     Declare C9A Scroll Cursor For                                                  
                     Select SPGVPS                                                                  
                     From Vtmspgf                                                                   
                     Where Spgcia = :Pcia And SPGZON = :PGEZNG                                      
                     And Spgcam = :PGECMP And                                                       
                     (Trim(Char(SPGCED)) = Trim(:Tced) Or                                           
                      Trim(Char(SPGCED)) = Trim(:Tcns))                                             
                     Order By Rrn(Vtmspgf) Desc                                                     
                     Fetch First 1 Row Only;                                                        
                                                                                                    
                     Exec Sql                                                                       
                      Open C9A;                                                                     
                                                                                                    
                     Exec Sql                                                                       
                      Fetch C9A Into :W_ValTra;                                                     
                                                                                                    
                     Exec Sql                                                                       
                       Close C9A;                                                                   
                  PGEVLP = W_ValTra;                                                                
                                                                                                    
                  Chain (OcedulaGc) LMLLST01;                                                       
                  If %Found(LMLLST01);                                                              
                    PGENMG = MLNAME;                                                                
                  Else;                                                                             
                    PGENMG = ' ';                                                                   
                  EndIf;                                                                            
                  PGEEST = 'P';                                                                     
                                                                                                    
                  PGEFEC = %date();                                                                 
                  PGEHOR = %time();                                                                 
                  PGEUSR =  User;                                                                   
                  PGECPG = PrCampaÑa1();                                                            
                  PGESEC = Spgsec;                                                                  
            If PGECPG = '2009';                                                                     
             PGECPG = '2008';                                                                       
            Endif;                                                                                  
                  If Prvtwpgef() = *Off;                                                            
                   Write REGPGE;                                                                    
                  Endif;                                                                            
                EndIf;                                                                              
               ReadE (Pcia:OcedulaG) Vtmipgf;                                                       
              EndDo;                                                                                
             EndIf;                                                                                 
             ReadE (Pcia:PZONA:PCAMPA) Vtmspgf;                                                     
            //EndIf;                                                                                
            EndDo;                                                                                  
          EndIf;                                                                                    
          Exec Sql                                                                                  
            Update Votrea00/Vtmspgf                                                                 
              Set SpgVps = 0                                                                        
              Where Spgcia = :Pcia And Spgcam >= :PCAMPA                                            
                  And Spginp = 'P' And SpgZon = :PZONA;                                             
        EndFor;                                                                                     
                                                                                                    
      /End-Free                                                                                     
     PPrInfPagEfec1    E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que cambia los estados registrados en "T" por "P"' //                       
       //------------------------------------------------------------------//                       
     PPrInfPagEfec2    B                                                                            
     DPrInfPagEfec2    Pi                                                                           
     DNrrAux           S              5i 0 Inz(*Zeros)                                              
     DIdx              S              5i 0 Inz(*Zeros)                                              
     DOcedulaG         S             15  0 Inz(*Zeros)                                              
     DOcedulaGc        S              9  0 Inz(*Zeros)                                              
     DWIpgcgc          S             15  0 Inz(*Zeros)                                              
     DUcuenta          S            100    Inz(' ')                                                 
      /Free                                                                                         
        NrrAux = Nrr2;                                                                              
        For idx = 1 to NrrAux;                                                                      
          Chain idx Rdatos2;                                                                        
                                                                                                    
          Exec Sql                                                                                  
          Update VOTREA00/Vtmspgf                                                                   
            Set SPGINP = 'P'                                                                        
          Where Spgcia = :Pcia And Spgcam >= :PCAMPA                                                
              And Spginp = 'T' And SpgZon = :PZONA;                                                 
                                                                                                    
          If sqlcode = *Zeros;                                                                      
            SetLl (Pcia:PZONA:PCAMPA) Vtmspgf;                                                      
            ReadE (Pcia:PZONA:PCAMPA) Vtmspgf;                                                      
            //Chain (Pcia:PZONA:PCAMPA) Vtmspgf;                                                    
            //If %Found(Vtmspgf) And SpgInp = 'P';                                                  
            Dow Not %Eof(Vtmspgf);                                                                  
             If SpgInp = 'P';                                                                       
              Ycia = Pcia;                                                                          
              Tced = %Trim(%Editc(SPGCED:'Z'));                                                     
              Tcns = *Blanks;                                                                       
              Conversion(Ycia:Tced:Tcns);                                                           
              OcedulaG = SPGCED;                                                                    
              OcedulaGc = *Zeros;                                                                   
              OcedulaGc = %Int(Tcns);                                                               
              Exec Sql                                                                              
                Select Ipgcgc Into :WIpgcgc                                                         
                From Votrea00/Vtmipgf                                                               
                Where Ipgcia = :Pcia And                                                            
                     (Trim(Char(Ipgcgc)) = Trim(:Tced) Or                                           
                     Trim(Char(Ipgcgc)) = Trim(:Tcns))                                              
                     And IpgEst = ' '                                                               
                     Order By RRN(Vtmipgf) Desc                                                     
                     fetch first 1 row Only;                                                        
              If WIpgcgc <> *Zeros;                                                                 
                OcedulaG = WIpgcgc;                                                                 
              Endif;                                                                                
              Setll (Pcia:OcedulaG) Vtmipgf;                                                        
              ReadE (Pcia:OcedulaG) Vtmipgf;                                                        
              Dow Not %Eof();                                                                       
                If (IPGEST = ' ' Or PrVtmgldf() = *On)                                              
                   And IPGNRC <> *Blanks;                                                           
                  PGECIA = Pcia;                                                                    
                  PGECDT = IPGCDC;                                                                  
                  Clear Ucuenta;                                                                    
                  Exec Sql                                                                          
                   Select Ipgnrc Into :Ucuenta                                                      
                   From Votrea00/Vtmipgf                                                            
                   Where Ipgcia = :Pcia And                                                         
                         Ipgcgc = :OcedulaG And                                                     
                         Ipgest = ' '                                                               
                   Fetch First 1 Row Only;                                                          
                  If Ucuenta <> ' ';                                                                
                   Ipgnrc = Ucuenta;                                                                
                  Endif;                                                                            
                  PGENRC = IPGNRC;                                                                  
                  Chain (Pcia:PGECDT) VTMTPCF;                                                      
                  If %Found(VTMTPCF);                                                               
                    PGEDST = TPCDES;                                                                
                  Else;                                                                             
                    PGEDST = ' ';                                                                   
                  EndIf;                                                                            
                                                                                                    
                  If Pgecdb = ' ';                                                                  
                   PGECDB = IPGCDB;                                                                 
                  Endif;                                                                            
                  Chain (Pcia:PGECDB) VTMBNSF;                                                      
                  If %Found(VTMBNSF);                                                               
                    PGEDSB = BNSDES;                                                                
                  Else;                                                                             
                    PGEDSB = ' ';                                                                   
                  EndIf;                                                                            
                                                                                                    
                  PGEZNG = Pzona;                                                                   
                  PGECMP = Pcampa;                                                                  
                  PGECDG = %Int(Tced);                                                              
                                                                                                    
                   Exec Sql                                                                         
                     Declare C9B Scroll Cursor For                                                  
                     Select SPGVPS                                                                  
                     From Vtmspgf                                                                   
                     Where Spgcia = :Pcia And SPGZON = :PGEZNG                                      
                     And Spgcam = :PGECMP And                                                       
                     (Trim(Char(SPGCED)) = Trim(:Tced) Or                                           
                      Trim(Char(SPGCED)) = Trim(:Tcns))                                             
                     Order By Rrn(Vtmspgf) Desc                                                     
                     Fetch First 1 Row Only;                                                        
                                                                                                    
                     Exec Sql                                                                       
                      Open C9B;                                                                     
                                                                                                    
                     Exec Sql                                                                       
                      Fetch C9B Into :W_ValTra;                                                     
                                                                                                    
                     Exec Sql                                                                       
                       Close C9B;                                                                   
                  PGEVLP = W_ValTra;                                                                
                                                                                                    
                  Chain (OcedulaGc) LMLLST01;                                                       
                  If %Found(LMLLST01);                                                              
                    PGENMG = MLNAME;                                                                
                  Else;                                                                             
                    PGENMG = ' ';                                                                   
                  EndIf;                                                                            
                  PGEEST = 'P';                                                                     
                                                                                                    
                  PGEFEC = %date();                                                                 
                  PGEHOR = %time();                                                                 
                  PGEUSR =  User;                                                                   
                  PGECPG = PrCampaÑa1();                                                            
                  PGESEC = Spgsec;                                                                  
            If PGECPG = '2009';                                                                     
             PGECPG = '2008';                                                                       
            Endif;                                                                                  
                  If Prvtwpgef() = *Off;                                                            
                   Write REGPGE;                                                                    
                  Endif;                                                                            
                EndIf;                                                                              
                ReadE (Pcia:OcedulaG) Vtmipgf;                                                      
              EndDo;                                                                                
             EndIf;                                                                                 
             ReadE (Pcia:PZONA:PCAMPA) Vtmspgf;                                                     
            //EndIf;                                                                                
            EndDo;                                                                                  
          EndIf;                                                                                    
          Exec Sql                                                                                  
            Update Votrea00/Vtmspgf                                                                 
              Set SpgVps = 0                                                                        
              Where Spgcia = :Pcia And Spgcam >= :PCAMPA                                            
                  And Spginp = 'P' And SpgZon = :PZONA;                                             
        EndFor;                                                                                     
                                                                                                    
                                                                                                    
      /End-Free                                                                                     
     PPrInfPagEfec2    E                                                                            
       //------------------------------------------------------------------//                       
       // Procedimiento que Lee las Opciones de la Pantalla                //                       
       //------------------------------------------------------------------//                       
     PFiltro           B                                                                            
     DFiltro           Pi                                                                           
                                                                                                    
      /Free                                                                                         
       Clear Rdato5;                                                                                
       Dow PPO = *Zeros;                                                                            
         Exfmt Rdato5;                                                                              
                                                                                                    
         If PPO <> *Zeros;                                                                          
           Select;                                                                                  
                                                                                                    
             When PPO   = 1;                                                                        
               // WFiltroEst = ' ';                                                                 
               WFiltroEst = '(' + $COMA + ' ' + $COMA + ')';                                        
                                                                                                    
             When PPO   = 2;                                                                        
               WFiltroEst = '(' + $COMA + 'T' + $COMA + ')';                                        
                                                                                                    
             When PPO   = 3;                                                                        
               WFiltroEst = '(' + $COMA + 'I' + $COMA + ')';                                        
                                                                                                    
             When PPO   = 4;                                                                        
               WFiltroEst = '(' + $COMA + 'R' + $COMA + ')';                                        
                                                                                                    
             When PPO   = 5;                                                                        
               clear WFiltroEst;                                                                    
                                                                                                    
             When PPO   = 6;                                                                        
               Leave;                                                                               
           Endsl;                                                                                   
         Endif;                                                                                     
       Enddo;                                                                                       
      /End-Free                                                                                     
     PFiltro           E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que consulta la inconsistencia                     //                       
       //------------------------------------------------------------------//                       
                                                                                                    
     PConsultar        B                                                                            
     DConsultar        Pi                                                                           
      /free                                                                                         
       Exec sql                                                                                     
         Select Sminf1, Spgcam, SpgZon, Sum(SpgVps)                                                 
         Into :PCced, :Dpgecmp, :Dgldzon, :PPdetde                                                  
         FROM Vtmspgf Inner Join Pslman On Spgzon = SMSLSP                                          
         WHERE Spgnbt = ' ' And Spgcia = :Pcia And Spgfnp <> :Tfecha                                
          And Spgcam = :Pcampa And Spgzon = :Pzona And Spgcam >=                                    
          :PcamR And Spginp In ('I', ' ' )                                                          
          Group By Sminf1, SpgCam, SpgZon                                                           
          Order by Sminf1, SpgCam, SpgZon;                                                          
                                                                                                    
       Chain (PCced) LMLLST01;                                                                      
       If %Found(LMLLST01);                                                                         
         PCNOMBRE = MLNAME;                                                                         
       Else;                                                                                        
         PCNOMBRE = ' ';                                                                            
       EndIf;                                                                                       
       DPGEFEC = PFECHAF;                                                                           
                                                                                                    
       If Spginp = ' ';                                                                             
        PDESTP = 'Ajuste Crédito';                                                                  
       Endif;                                                                                       
       If Spginp = 'I';                                                                             
        PDESTP = 'Inconsis Pago';                                                                   
       Endif;                                                                                       
       If Spginp = 'T';                                                                             
        PDESTP = 'P en Efectivo';                                                                   
       Endif;                                                                                       
       If Spginp = 'R';                                                                             
        PDESTP = 'Pago Realizado';                                                                  
       Endif;                                                                                       
       If Spginp <> ' ' And Spginp <> 'T' And Spginp <> 'I'                                         
          And Spginp <> 'R';                                                                        
        PDESTP = *Blanks;                                                                           
       Endif;                                                                                       
                                                                                                    
       Dow *In03= *Off;                                                                             
         Exfmt Rdato13;                                                                             
         If *In03= *ON;                                                                             
           *In03= *OFF;                                                                             
           //Posix = Posi;                                                                          
           Leave;                                                                                   
         Endif;                                                                                     
       Enddo;                                                                                       
      /End-Free                                                                                     
     PConsultar        E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Genera Posteo Automatico de Notas Debito en la //                       
       // Cartera a Favor de las Guías                                     //                       
       //------------------------------------------------------------------//                       
     PGenerarposteoCartera1...                                                                      
     P                 B                                                                            
     DGenerarposteoCartera1...                                                                      
     D                 Pi                                                                           
     DW_indicador                     1A                                                            
      /Free                                                                                         
       If Nrr > *Zeros;                                                                             
       Clear Rdato4;                                                                                
       Dow Popc = *Blanks;                                                                          
        Exfmt Rdato4;                                                                               
        If Popc = 'S' Or Popc = 's';                                                                
         AfectarCartera1(W_indicador);                                                              
         //PrFTP();                                                                                 
         *In36 = *On;                                                                               
         Exfmt Rdato4;                                                                              
         *In36 = *Off;                                                                              
         Exfmt RDATO8;                                                                              
        Endif;                                                                                      
       Enddo;                                                                                       
       Endif;                                                                                       
      /End-Free                                                                                     
     PGenerarposteoCartera1...                                                                      
     P                 E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Graba Novedades para Afectar la Cartera        //                       
       //------------------------------------------------------------------//                       
                                                                                                    
     PAfectarCartera1  B                                                                            
     DAfectarCartera1  Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DW_IndLLamado                    1A                                                            
     DTsw              s              1a   Inz(*Blanks)                                             
     DWsig             s              2a   Inz(*Blanks)                                             
     DWano             s              2a   Inz(*Blanks)                                             
     DWaÑo             s              4a   Inz(*Blanks)                                             
     DWmes             s              2a   Inz(*Blanks)                                             
     DWdia             s              2a   Inz(*Blanks)                                             
     DKban             s              3a   Inz(*Blanks)                                             
     DKced             s             15s 0 Inz(*Zeros)                                              
     DKtip             s              2a   Inz(*Blanks)                                             
     DCmd              s           3000a                                                            
     DLen              s             15p 5                                                          
     DWvari            s            200a   Inz(*Blanks)                                             
     Didx              s              1S 0 Inz(*Zeros)                                              
                                                                                                    
     ** Definiciòn de Prototipos de Trabajo                                                         
     DQcmdexc          Pr                  Extpgm('QCMDEXC')                                        
     D                             3000a   Const Options(*Varsize)                                  
     D                               15p 5 Const                                                    
     D                                3a   Const Options(*Nopass)                                   
      /Free                                                                                         
       W_IndFtp = 'N';                                                                              
       Evalr Wsig = %Char(Sig_sys);                                                                 
       Wsig = %Xlate(' ':'0':Wsig);                                                                 
       Evalr Wano = %Char(AÑo_sys);                                                                 
       Wano = %Xlate(' ':'0':Wano);                                                                 
       WaÑo = Wsig + Wano;                                                                          
       Evalr Wmes = %Char(Mes_sys);                                                                 
       Wmes = %Xlate(' ':'0':Wmes);                                                                 
       Evalr Wdia = %Char(Dia_sys);                                                                 
       Wdia = %Xlate(' ':'0':Wdia);                                                                 
                                                                                                    
         If Pop = *Blanks;                                                                          
           If Ptipo = 'P en Efectivo';                                                              
             SqlStr = 'Select * ' +                                                                 
                   'From Votrea00/Vtmspgf ' +                                                       
                   'Where Spgcia = ' + '''' + Pcia + '''' + ' And ' +                               
                         'Spgnbt = ' + '''' + Blan + '''' + ' And ' +                               
                         '((Spgfnp <> ' + '''' + Tfecha + '''' + ') Or ' +                          
                         '(Spgfnp = ' + '''' + Blan + '''' + ')) And ' +                            
                         '(Spginp = ''T''' +                                                        
                         ') And ' +                                                                 
                         'Spgvps <> 0 And Spgcam >=' + '''' + PcamR + '''';                         
           Else;                                                                                    
             SqlStr = 'Select * ' +                                                                 
                   'From Votrea00/Vtmspgf ' +                                                       
                   'Where Spgcia = ' + '''' + Pcia + '''' + ' And ' +                               
                         'Spgnbt = ' + '''' + Blan + '''' + ' And ' +                               
                         '((Spgfnp <> ' + '''' + Tfecha + '''' + ') Or ' +                          
                         '(Spgfnp = ' + '''' + Blan + '''' + ')) And ' +                            
                         '(Spginp = '' ''' + ' Or Spginp = ''I''' +                                 
                         ') And ' +                                                                 
                         'Spgvps <> 0 And Spgcam >=' + '''' + PcamR + '''';                         
           EndIf;                                                                                   
         Endif;                                                                                     
                                                                                                    
         If Pop = '3';                                                                              
           If Ptipo = 'P en Efectivo';                                                              
             SqlStr = 'Select * ' +                                                                 
                   'From Votrea00/Vtmspgf ' +                                                       
                   'Where Spgcia = ' + '''' + Pcia + '''' + ' And ' +                               
                         'Spgnbt = ' + '''' + Blan + '''' + ' And ' +                               
                         '((Spgfnp <> ' + '''' + Tfecha + '''' + ') Or ' +                          
                         '(Spgfnp = ' + '''' + Blan + '''' + ')) And ' +                            
                         'Spgzon = ' + '''' + Pzona + '''' + ' And ' +                              
                         '(Spginp = ''T''' +                                                        
                         ') And ' +                                                                 
                         'Spgvps <> 0 And Spgcam =' + '''' + Pcampa + '''';                         
           Else;                                                                                    
             SqlStr = 'Select * ' +                                                                 
                   'From Votrea00/Vtmspgf ' +                                                       
                   'Where Spgcia = ' + '''' + Pcia + '''' + ' And ' +                               
                         'Spgnbt = ' + '''' + Blan + '''' + ' And ' +                               
                         '((Spgfnp <> ' + '''' + Tfecha + '''' + ') Or ' +                          
                         '(Spgfnp = ' + '''' + Blan + '''' + ')) And ' +                            
                         'Spgzon = ' + '''' + Pzona + '''' + ' And ' +                              
                         '(Spginp = '' ''' + ' Or Spginp = ''I''' +                                 
                         ') And ' +                                                                 
                         'Spgvps <> 0 And Spgcam =' + '''' + Pcampa + '''';                         
           EndIf;                                                                                   
         Endif;                                                                                     
                                                                                                    
         Exec Sql                                                                                   
          Prepare Cur3 From :SqlStr;                                                                
                                                                                                    
         Exec Sql                                                                                   
          Declare Cursor3 Cursor For Cur3;                                                          
                                                                                                    
         Exec Sql                                                                                   
          Open Cursor3;                                                                             
                                                                                                    
         Exec Sql                                                                                   
          Fetch Cursor3 Into :Fields2;                                                              
                                                                                                    
          If Sqlcod <> 100 And Sqlcod >= *Zeros;                                                    
           Conse = *Zeros;                                                                          
           Consea = *Blanks;                                                                        
           Conseb = *Blanks;                                                                        
           PrConsecutivo();                                                                         
           Evalr Consea = %Char(Conse);                                                             
           Consea = %Xlate(' ':'0':Consea);                                                         
           Conseb = %Subst(Consea:2:5);                                                             
          Endif;                                                                                    
          Dow Sqlcod <> 100 And Sqlcod >= *Zeros;                                                   
          If PrVerificarRutGuia() = *Off;                                                           
           Tced = %Trim(%Editc(xSpgced:'Z'));                                                       
           If xSpginp = 'T' And PrVtmipgf(Tced) = *On;                                              
            For idx = 1 to 2;                                                                       
             Ycia = Pcia;                                                                           
             Tced = %Trim(%Editc(xSpgced:'Z'));                                                     
             Tcns = *Blanks;                                                                        
             Conversion(Ycia:Tced:Tcns);                                                            
             Icedula = *Zeros;                                                                      
             If %Len(%Trim(Tcns)) > *Zeros;                                                         
               Icedula = %Int(Tcns);                                                                
             Else;                                                                                  
               Icedula = xSpgced;                                                                   
             Endif;                                                                                 
             Tsw = 'S';                                                                             
             Kban = *Blanks;                                                                        
             Kced = xSpgced;                                                                        
             Kord = *Zeros;                                                                         
             PrBuscarOrden();                                                                       
             If idx = 1;                                                                            
               Ktip = 'A-';                                                                         
               Wcbval = xSpgvps * (-1);                                                             
             Else;                                                                                  
               Ktip = 'A+';                                                                         
               Wcbval = xSpgvps;                                                                    
             EndIf;                                                                                 
             If %Open(Vtwcabf2) = '0';                                                              
               Open Vtwcabf2;                                                                       
             Endif;                                                                                 
             Chain (Kban:Icedula:Kord:Ktip) Vtwcabf2;                                               
             If Not %Found(Vtwcabf2);                                                               
               Wcbced = xSpgced;                                                                    
               Wcbtip = Ktip;                                                                       
               Wcbord = Kord;                                                                       
               Wcbbco = *Blanks;                                                                    
               If %Len(%Trim(Htexto)) <= *Zeros;                                                    
                Wcbcmn = 'DctoCdLE' + %Trim(PrCampaÑa1()) + 'mt' +                                  
                         %Trim(xSpgcam);                                                            
               Else;                                                                                
                Wcbcmn = 'DctoCdLE' + %Trim(PrCampaÑa1()) + 'mt' +                                  
                         %Trim(xSpgcam);                                                            
               Endif;                                                                               
               Wcbfet = WaÑo + '/' + Wmes + '/' + Wdia;                                             
               Wcbsts = *Blanks;                                                                    
               Wcbusr = User;                                                                       
               Wcbcct = 043;                                                                        
               PrCerrarPost();                                                                      
               Select;                                                                              
                 When W_IndLLamado = '1';                                                           
                   PrInfPagEfec();                                                                  
                 When W_IndLLamado = '2';                                                           
                   PrInfPagEfec1();                                                                 
                 When W_IndLLamado = '3';                                                           
                   PrInfPagEfec2();                                                                 
               EndSl;                                                                               
                                                                                                    
              PrActualizar();                                                                       
                                                                                                    
              PagoG = *Blanks;                                                                      
              PagoG = %Trim(%Editc(Wcbced:'Z')) + '-' +                                             
                      %Trim(%Editc(Wcbval:'K')) + 'I';                                              
                                                                                                    
              Write Rwcab2;                                                                         
              PrPremiosEspeciales();                                                                
              PrVtwpgif();                                                                          
              If Idx = 1;                                                                           
                 Clear Fields4;                                                                     
                 Qlpgcia =XSpgCia;                                                                  
                 Qlpgcmp =XSpgcam;                                                                  
                 Qlpgzna =XSPGZon;                                                                  
                 Qlpgsec =XSPGSec;                                                                  
                 Qlpgcdg =XSpgced;                                                                  
                 Qlpgvde = XSpgvds;                                                                 
                 QLPGFDE = xSpgfbt;                                                                 
                // Recupera Región se usan Variables Temporales                                     
                   w_TempCia=Pcia;                                                                  
                   w_TempZon=Spgzoni;                                                               
                   Pcia     = QLPgCia;                                                              
                   Spgzoni  = QLPgZna;                                                              
                   QlpgReg  = PrRegion();                                                           
                   Pcia     = w_TempCia;                                                            
                   Spgzoni  = w_TempZon;                                                            
                //fin recupera Región                                                               
                 Qlpgmot= Pprmot;                                                                   
                 PrRecuperaPagosL();                                                                
                 PrVtwlpgf();                                                                       
              Endif;                                                                                
             If Tsw <> *Blanks;                                                                     
              PrVtwcabf2(Pcia);                                                                     
              Tsw = *blanks;                                                                        
              Wvari = *Blanks;                                                                      
              Wvari = 'SBMJOB CMD(CALL PGM(VOTREP00/VTNCBI0L) PARM(' +                              
              '''' + Pcia + '''' + '))' +                                                           
              ' JOB(NOVED_CART) JOBQ(GENERP00/PRCGUIAS)';                                           
              Cmd = %Trim(Wvari);                                                                   
              Callp(E) Qcmdexc(Cmd:%Size(Cmd));                                                     
              PrActualizarVtedndf();                                                                
             Endif;                                                                                 
            Endif;                                                                                  
            If %Open(Vtwcabf2) = '1';                                                               
             Close Vtwcabf2;                                                                        
            Endif;                                                                                  
           EndFor;                                                                                  
            W_IndFtp = 'S';                                                                         
           //PrFTP();                                                                               
          ElseIf xSpginp = *Blanks Or xSpginp = 'I' Or xSpginp = 'i';                               
            Ycia = Pcia;                                                                            
            Tced = %Trim(%Editc(xSpgced:'Z'));                                                      
            Tcns = *Blanks;                                                                         
            Conversion(Ycia:Tced:Tcns);                                                             
            Icedula = *Zeros;                                                                       
            If %Len(%Trim(Tcns)) > *Zeros;                                                          
             Icedula = %Int(Tcns);                                                                  
            Else;                                                                                   
             Icedula = xSpgced;                                                                     
            Endif;                                                                                  
            Tsw = 'S';                                                                              
            Kban = *Blanks;                                                                         
            Kced = xSpgced;                                                                         
            Kord = *Zeros;                                                                          
            PrBuscarOrden();                                                                        
            Ktip = 'A-';                                                                            
            If %Open(Vtwcabf2) = '0';                                                               
             Open Vtwcabf2;                                                                         
            Endif;                                                                                  
            Chain (Kban:Icedula:Kord:Ktip) Vtwcabf2;                                                
            If Not %Found(Vtwcabf2);                                                                
             Wcbced = xSpgced;                                                                      
             Wcbtip = Ktip;                                                                         
             Wcbval = xSpgvps * (-1);                                                               
             Wcbord = Kord;                                                                         
             Wcbbco = *Blanks;                                                                      
             Wcbcmn = 'DctoCdLP' + %Trim(PrCampaÑa1()) + 'mt' +                                     
                       %Trim(xSpgcam);                                                              
             Wcbfet = WaÑo + '/' + Wmes + '/' + Wdia;                                               
             Wcbsts = *Blanks;                                                                      
             Wcbusr = User;                                                                         
             Wcbcct = xSpgccr;                                                                      
             If Wcbcct = *Zeros;                                                                    
              Wcbcct = 042;                                                                         
             Endif;                                                                                 
             PrCerrarPost();                                                                        
             PrActualizar();                                                                        
                                                                                                    
             PagoG = *Blanks;                                                                       
             PagoG = %Trim(%Editc(Wcbced:'Z')) + '-' +                                              
                     %Trim(%Editc(Wcbval:'K')) + 'I';                                               
                                                                                                    
             Write Rwcab2;                                                                          
             PrPremiosEspeciales();                                                                 
             PrVtwpgif();                                                                           
            If Tsw <> *Blanks;                                                                      
             PrVtwcabf2(Pcia);                                                                      
             Tsw = *blanks;                                                                         
             Wvari = *Blanks;                                                                       
             Wvari = 'SBMJOB CMD(CALL PGM(VOTREP00/VTNCBI0L) PARM(' +                               
             '''' + Pcia + '''' + '))' +                                                            
             ' JOB(NOVED_CART) JOBQ(GENERP00/PRCGUIAS)';                                            
             Cmd = %Trim(Wvari);                                                                    
             Callp(E) Qcmdexc(Cmd:%Size(Cmd));                                                      
             PrActualizarVtedndf();                                                                 
                                                                                                    
             Pcedula = Wcbced;                                                                      
             Pvalor = Wcbval;                                                                       
             Pcampa = xSpgcam;                                                                      
             PgmMensajeTexto(PtipoW:PCia:Pcedula:Pvalor:Pcampa);                                    
            Endif;                                                                                  
           Endif;                                                                                   
           If %Open(Vtwcabf2) = '1';                                                                
            Close Vtwcabf2;                                                                         
           Endif;                                                                                   
          Endif;                                                                                    
          Endif;                                                                                    
         Exec Sql                                                                                   
          Fetch Cursor3 Into :Fields2;                                                              
         Enddo;                                                                                     
                                                                                                    
         Exec Sql                                                                                   
          Close Cursor3;                                                                            
                                                                                                    
         If W_IndFtp = 'S';                                                                         
           PrFTP();                                                                                 
         EndIf;                                                                                     
                                                                                                    
      /End-Free                                                                                     
     PAfectarCartera1  E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que se Encarga de Enviar los pagos al banco        //                       
       //------------------------------------------------------------------//                       
     PEnviar           B                                                                            
     DEnviar           Pi                                                                           
      /Free                                                                                         
       W_INdL = '1';                                                                                
       GenerarposteoCartera1(W_INdL);                                                               
      /End-Free                                                                                     
     PEnviar           E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que se Encarga de Enviar los pagos al banco        //                       
       //------------------------------------------------------------------//                       
     PEnviar1          B                                                                            
     DEnviar1          Pi                                                                           
      /Free                                                                                         
       W_INdL = '2';                                                                                
       GenerarposteoCartera1(W_INdL);                                                               
      /End-Free                                                                                     
     PEnviar1          E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que se Encarga de Enviar los pagos al banco        //                       
       //------------------------------------------------------------------//                       
                                                                                                    
     PEnviar2          B                                                                            
     DEnviar2          Pi                                                                           
      /Free                                                                                         
       W_INdL = '3';                                                                                
       GenerarposteoCartera1(W_INdL);                                                               
      /End-Free                                                                                     
     PEnviar2          E                                                                            
                                                                                                    
      //------------------------------------------------------------------//                        
      // Procedimiento que valida la Dtaara                               //                        
      //------------------------------------------------------------------//                        
                                                                                                    
     PPrDtaara1        B                                                                            
     DPrDtaara1        Pi                                                                           
     DXfecha           s             10a   Inz(*Blanks)                                             
     DOfecha           s             10a   Inz(*Blanks)                                             
      /Free                                                                                         
       Ofecha = %Char(%Date());                                                                     
       // Archivo de Excepciones Días Pagos en Efectivo Guías                                       
       Exec Sql                                                                                     
        Select Epgfpe Into :Xfecha                                                                  
        From Votrea00/Vtmepgf                                                                       
        Where Epgcia = :Pcia And :Ofecha Between Epgfej And Epgfpe;                                 
                                                                                                    
       If SqlCod <> 100 And SqlCod >= *Zeros;                                                       
        fecha2 = Xfecha;                                                                            
        fecha2 = %Trim(%ScanRpl('-':'':Fecha2));                                                    
       Else;                                                                                        
        fecha2 = %Char(%Date():*Iso0);                                                              
        PgmDia(Fecha2:Udia);                                                                        
        Select;                                                                                     
          When Udia = 'Lunes';                                                                      
             Dias_sum = 4;                                                                          
                                                                                                    
          When Udia = 'Martes';                                                                     
             Dias_sum = 3;                                                                          
                                                                                                    
          When Udia = 'Miercoles';                                                                  
             Dias_sum = 2;                                                                          
                                                                                                    
          When Udia = 'Jueves';                                                                     
             Dias_sum = 1;                                                                          
                                                                                                    
         EndSl;                                                                                     
         Strdate = %Date(Fecha2:*Iso0);                                                             
         Enddate = Strdate + %Days(Dias_Sum);                                                       
         fecha2 = %Trim(%ScanRpl('-':'':%Char(Enddate)));                                           
        Endif;                                                                                      
                                                                                                    
        In *Lock MyDtaara1;                                                                         
        FechaGen = %Subst(Nomb_arc:1:8);                                                            
                                                                                                    
        If FechaGen = fecha2;                                                                       
          W_Conse = %Int(%Subst(Nomb_arc:9:3));                                                     
          W_Conse = W_Conse + 1;                                                                    
          Nomb_arc = %Trim(fecha2) + %EditC(W_conse:'X');                                           
        Else;                                                                                       
          Nomb_arc = %Trim(fecha2) + '001';                                                         
        EndIf;                                                                                      
                                                                                                    
        Out MyDtaara1;                                                                              
      /End-Free                                                                                     
     PPrDtaara1        E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Pinta el Sub Archivo de la Pantalla            //                       
       //------------------------------------------------------------------//                       
     PPintarSflDE      B                                                                            
     DPintarSflDE      Pi                                                                           
      /Free                                                                                         
       Dow Salir = *Off;                                                                            
        If Pzona <> *Blanks;                                                                        
          W_ZonaAux = Pzona;                                                                        
        EndIf;                                                                                      
        LlenarSfldE();                                                                              
        //If Posix > *Zeros;                                                                        
        // Posic = Posix;                                                                           
        //Endif;                                                                                    
        Ppais = Xpais;                                                                              
        SflDspCtl = *On;                                                                            
        Write Rteclas;                                                                              
        Exfmt Rcontrol2;                                                                            
        SflDspCtl = *Off;                                                                           
        If Posteo = *On And Salir = *Off and *In39 = *On;                                           
         GenerarposteoCartera();                                                                    
        Endif;                                                                                      
                                                                                                    
        //Envia Pagos al Banco                                                                      
        If EnviarPag = *On and *In41 = *On;                                                         
          Pzona = ZonaAux;                                                                          
          If EnviarPag = *On;                                                                       
            Pop = '3';                                                                              
          EndIf;                                                                                    
          Enviar2();                                                                                
          Iter;                                                                                     
        Endif;                                                                                      
                                                                                                    
       //Realiza filtros de la información                                                          
         If Filtrar = *On;                                                                          
           Filtro();                                                                                
           Leave;                                                                                   
         Endif;                                                                                     
                                                                                                    
        If Salir = *Off And Nrr2 > *Zeros;                                                          
        Readc Rdatos2;                                                                              
         Dow Not %Eof();                                                                            
          Select;                                                                                   
           When Pop = ' ';                                                                          
          Endsl;                                                                                    
          Pop = *Blanks;                                                                            
          Update Rdatos2;                                                                           
         Readc Rdatos2;                                                                             
        Enddo;                                                                                      
        Endif;                                                                                      
       Enddo;                                                                                       
       If Salir = *On;                                                                              
        Salir = *Off;                                                                               
       Endif;                                                                                       
      /End-Free                                                                                     
     PPintarSflDE      E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Llena el Sub Archivo del Sub File              //                       
       //------------------------------------------------------------------//                       
     PLlenarSflDE      B                                                                            
     DLlenarSflDE      Pi                                                                           
     DIdx              S              5i 0 Inz(*Zeros)                                              
      /Free                                                                                         
       BorraSfldE();                                                                                
       Exec Sql                                                                                     
        Declare C4 Scroll Cursor For                                                                
        Select Spgcia, Spgsec, Spgtrr, Spgzon, Spgcam, Spgvps, Spgfbt,                              
               Spgced, Spginp                                                                       
        From Vtmspgf                                                                                
        Where Spgnbt = ' ' And Spgcia = :Pcia And Spgfnp <> :Tfecha                                 
              And Spgcam = :Pcampa And Spgzon = :W_ZonaAux                                          
              And Spgcam >= :PcamR And                                                              
              //Spginp In (' ', 'T', 'I')                                                           
              Spginp In ('T')                                                                       
        Order By Spgcia, Spgcam, Spgzon, Spgsec, Spgtrr, Spgfbt;                                    
                                                                                                    
       Exec Sql                                                                                     
        Open C4;                                                                                    
                                                                                                    
       Exec Sql                                                                                     
        Fetch C4 Into :Fields1;                                                                     
                                                                                                    
        Pcampa1 = RSpgcam;                                                                          
        RTotales = *Zeros;                                                                          
        Dow Sqlcod <> 100 And Sqlcod >= *Zeros;                                                     
          For idx = 1 to 2;                                                                         
           *In64 = *Off;                                                                            
           If RSpgvps > *Zeros;                                                                     
            *In60 = *Off;                                                                           
            If RSpginp <> *Blanks And RSpginp <> 'T';                                               
             *In60 = *On;                                                                           
            Endif;                                                                                  
            Pop = *Blanks;                                                                          
            Pzona = RSpgzon;                                                                        
            If idx = 1;                                                                             
              Pst2 = '043+';                                                                        
            Else;                                                                                   
              Pst2 = '043-';                                                                        
            EndIf;                                                                                  
            Pcedula1 = RSpgced;                                                                     
            Pnombre1 = PrNombreGuia();                                                              
            If idx = 1;                                                                             
              Pvalor1 = RSpgvps;                                                                    
            else;                                                                                   
              Pvalor1 = RSpgvps * -1;                                                               
            EndIf;                                                                                  
            RTotales = RTotales + Pvalor1;                                                          
            Ptr1 = RSpgtrr;                                                                         
            Nrr2 += 1;                                                                              
            Posi = Nrr2;                                                                            
            Write Rdatos2;                                                                          
           Endif;                                                                                   
         EndFor;                                                                                    
       Exec Sql                                                                                     
        Fetch C4 Into :Fields1;                                                                     
        Enddo;                                                                                      
        ZonaAux = Pzona;                                                                            
        If Nrr2 > *Zeros;                                                                           
         *In62 = *On;                                                                               
         *In64 = *On;                                                                               
          Pop = *Blanks;                                                                            
          Pzona = *Blanks;                                                                          
          Pst2 = *Blanks;                                                                           
          Pcedula1 = *Zeros;                                                                        
          Pnombre1 = 'Total...............';                                                        
          Pvalor1 = RTotales;                                                                       
          Ptr1 = *Blanks;                                                                           
          Nrr2 += 1;                                                                                
          Write Rdatos2;                                                                            
          *In62 = *Off;                                                                             
          *In64 = *Off;                                                                             
        Endif;                                                                                      
                                                                                                    
       Exec Sql                                                                                     
        Close C4;                                                                                   
                                                                                                    
        SflEnd = *On;                                                                               
        If Nrr2 > *Zeros;                                                                           
         SflDsp = *On;                                                                              
        Else;                                                                                       
         SflDsp = *Off;                                                                             
        Endif;                                                                                      
      /End-Free                                                                                     
     PLlenarSflDE      E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Borra el Sub Archivo de la Pantalla            //                       
       //------------------------------------------------------------------//                       
     PBorraSflDE       B                                                                            
     DBorraSflDE       Pi                                                                           
      /Free                                                                                         
       Posic = 1;                                                                                   
       Nrr2 = *Zeros;                                                                               
       SflClr = *On;                                                                                
       Write Rcontrol2;                                                                             
       SflClr = *Off;                                                                               
      /End-Free                                                                                     
     PBorraSflDE       E                                                                            
                                                                                                    
                                                                                                    
      //------------------------------------------------------------------//                        
      // Procedimiento que envia al FTP                                   //                        
      //------------------------------------------------------------------//                        
     PPrFTP            B                                                                            
     DPrFTP            Pi                                                                           
      /Free                                                                                         
                                                                                                    
       PrRevalidarCuentas();                                                                        
                                                                                                    
       Wlib = 'VOTREA' + PCIA;                                                                      
                                                                                                    
       Sqlstr1 =  'DELETE FROM ' + %Trim(Wlib) + '/VTWDPRF ' +                                      
                  'Where DprNmp = ''VTPSGB0R''';                                                    
                                                                                                    
       Exec Sql                                                                                     
        Prepare Curd From :SqlStr1;                                                                 
                                                                                                    
       Exec Sql                                                                                     
        Execute Curd;                                                                               
                                                                                                    
       Sqlstr1 =  'DELETE FROM ' + %Trim(Wlib) + '/VTWEPRF';                                        
                                                                                                    
       Exec Sql                                                                                     
        Prepare Curd From :SqlStr1;                                                                 
                                                                                                    
       Exec Sql                                                                                     
        Execute Curd;                                                                               
                                                                                                    
                                                                                                    
       PrDtaara1();                                                                                 
       fecha3 = %subst(Fecha2:1:8);                                                                 
       Fecha1 = %int(fecha3);                                                                       
       SetLl *Loval VTWPGEF;                                                                        
       Read VTWPGEF;                                                                                
       Dow Not %Eof(VTWPGEF);                                                                       
         If PGEEST = 'P';                                                                           
           BancoProv = ' ';                                                                         
           Chain (PCIA:PGECDB)VTMBNSF;                                                              
           If %Found(VTMBNSF);                                                                      
             BancoProv = BNSCCB;                                                                    
           EndIf;                                                                                   
           Clear TipoTrasn;                                                                         
           If PGECDT = 'CC';                                                                        
             TipoTrasn = 27;                                                                        
           ElseIf PGECDT = 'CA';                                                                    
             TipoTrasn = 37;                                                                        
           endIf;                                                                                   
                                                                                                    
           Wsql2 ='Insert into ' + %trim(Wlib) + '/' + 'VTWDPRF ' +                                 
                 ' Values ('+                                                                       
                 ' 6, ' +                                                                           
                 $Coma + %Trim(%Char(PGECDG)) + $coma + ', '  +                                     
                 $coma + %Subst(PGENMG:1:30) + $coma + ', '  +                                      
                 %trim(BancoProv) + ', '  +                                                         
                 $Coma + %trim(PGENRC) + $Coma + ', '  +                                            
                 $COMA + ' ' + $COMA + ', '  +                                                      
                 %Char(TipoTrasn) + ', '  +                                                         
                 %Trim(%Char(PGEVLP)) + ', '  +                                                     
                 %Char(Fecha1) + ', '  +                                                            
                 $COMA + ' ' + $COMA + ', '  +                                                      
                 $COMA + ' ' + $COMA + ', '  +                                                      
                 '0' + ', '  +                                                                      
                 $COMA + ' ' + $COMA + ', '  +                                                      
                 $COMA + ' ' + $COMA + ', '  +                                                      
                 $COMA + ' ' + $COMA + ', '  +                                                      
                 $COMA + ' ' + $COMA + ', '  +                                                      
                 $COMA + 'VTPSGB0R' + $COMA + ') ';                                                 
                                                                                                    
           Exec Sql Execute Immediate :wsql2;                                                       
                                                                                                    
           PGEEST = 'V';                                                                            
           PGECPG = PrCampaÑa1();                                                                   
            If PGECPG = '2009';                                                                     
             PGECPG = '2008';                                                                       
            Endif;                                                                                  
           Update REGPGE;                                                                           
                                                                                                    
           Ycia = Pcia;                                                                             
           Tced = %Trim(%Editc(Pgecdg:'Z'));                                                        
           Tcns = *Blanks;                                                                          
           Conversion(Ycia:Tced:Tcns);                                                              
                                                                                                    
           Clear Xnro;                                                                              
           Exec Sql                                                                                 
            Select Count(0) Into :Xnro                                                              
            From Votrea00/Vtmgldf                                                                   
            Where Gldcia = :Pcia And Gldcte = Trim(:Tcns) And                                       
                  Gldest = ' ';                                                                     
                                                                                                    
           Clear Ynro;                                                                              
           Exec Sql                                                                                 
            Select Count(0) Into :Ynro                                                              
            From Votrea00/Vtmgldf                                                                   
            Where Gldcia = :Pcia And Gldcte = Trim(:Tcns) And                                       
                  Gldest = 'D';                                                                     
           If Ynro > 0 And Xnro <= 0;                                                               
            Exec Sql                                                                                
             Delete From Votrea00/Vtmipgf                                                           
             Where Ipgcia = :Pcia And Ipgcgc = :Pgecdg;                                             
           Endif;                                                                                   
         EndIf;                                                                                     
         Read VTWPGEF;                                                                              
       EndDo;                                                                                       
                                                                                                    
       Wsql3 ='Select Count(0), Sum(DPRVAL) from '+%trim(Wlib)+'/'+'VTWDPRF ' +                     
              'Where DPRNMP = ''VTPSGB0R''';                                                        
                                                                                                    
       Exec Sql Prepare C8 From :Wsql3;                                                             
       Exec Sql Declare C3 Scroll Cursor For C8;                                                    
       Exec Sql Open C3;                                                                            
       Exec Sql Fetch C3 Into :W_CONT, :W_ValT;                                                     
       Exec Sql Close C3;                                                                           
                                                                                                    
       Wsql4 ='Insert into ' + %trim(Wlib) + '/' + 'VTWEPRF ' + ' Values ('+                        
             '1, ' +                                                                                
             '000000800100159, ' +                                                                  
             $COMA + 'N' + $COMA + ', '  +                                                          
             $COMA + ' ' + $COMA + ', '  +                                                          
             '220, ' +                                                                              
             $COMA + 'Pagos' + $COMA + ', '  +                                                      
             %Char(Fecha1) + ', '  +                                                                
             $COMA + 'G' + $COMA + ', '  +                                                          
             %Char(Fecha1) + ', '  +                                                                
             %Char(W_Cont) + ', '  +                                                                
             '0, ' +                                                                                
             %Char(W_ValT) + ', '  +                                                                
             '2910015901 ' + ', '  +                                                                
             $COMA + 'D' + $COMA + ', '  +                                                          
             $COMA + ' ' + $COMA + ') ';                                                            
                                                                                                    
       Exec Sql Execute Immediate :wsql4;                                                           
                                                                                                    
       Wsql5 ='Create table Qtemp/PG' + %Char(W_Conse) + ' As ('+                                   
             'SELECT SUBSTR(EPRTIP||DIGITS(EPRNIT)||EPRAPL||' +                                     
             'EPRFI1||DIGITS(EPRCLA)||EPRDES||' +                                                   
             'EPRFEC||EPRSEC||EPRFAP||DIGITS(EPRNUM)||DIGITS(EPRVRD)||' +                           
             'DIGITS(DEC(EPRVRC, 17, 2))||DIGITS(EPRCTA)||EPRTCC||' +                               
             'EPRFI2, 1, 262) ' +                                                                   
             'AS REGISTRO ' +                                                                       
             'FROM ' + %trim(Wlib) + '/' + 'VTWEPRF ) With Data';                                   
                                                                                                    
                                                                                                    
       Exec Sql Execute Immediate :wsql5;                                                           
                                                                                                    
       Wsql6 ='Insert into Qtemp/PG' + %Char(W_Conse) + ' Select '+                                 
             'DPRTIP||CHAR(DIGITS(DEC(DPRNIT)))||DPRNOM||DIGITS(DPRBCT)' +                          
             '||DPRCTA||DPRIND||DIGITS(DPRTPT)' +                                                   
             '||DIGITS(DEC(DPRVAL, 17, 2))||DIGITS(DPRFEA)' +                                       
             '||DPRREF||DPRTDI||DIGITS(DPROFE)' +                                                   
             '||DPRNMF||DPREMA||DPRNIA||DPRFIL ' +                                                  
             'FROM ' + %trim(Wlib) + '/' + 'VTWDPRF ' +                                             
             'Where DprNmp = ''VTPSGB0R''';                                                         
                                                                                                    
       Exec Sql Execute Immediate :wsql6;                                                           
                                                                                                    
       PindIFS = 'N';                                                                               
       PcodApli = 1;                                                                                
       PdesPli = 'NR';                                                                              
       Plibre = 'QTEMP';                                                                            
       Pextension = 'TXT';                                                                          
       ParchivoOri = 'PG' + %Char(W_Conse);                                                         
       ParchivoDes = 'PG' + Nomb_arc;                                                               
                                                                                                    
       PgmFtp(PindIFS                                                                               
              :PcodApli                                                                             
              :PdesPli                                                                              
              :Pextension                                                                           
              :ParchivoOri                                                                          
              :ParchivoDes                                                                          
              :Plibre);                                                                             
                                                                                                    
       //------------------------------------------------------------------//                       
       // Se envia la notificación a los usuarios registrados              //                       
       //------------------------------------------------------------------//                       
                                                                                                    
               Clear Pdestinatario;                                                                 
                                                                                                    
               //Recupera Email a enviar                                                            
               Setll (PCIA:'R02') VTMNTEF;                                                          
               Reade (PCIA:'R02') VTMNTEF;                                                          
               Dow Not %Eof(VTMNTEF);                                                               
                 If %Found(VTMNTEF) And NTEEST = 'A';                                               
                   Pdestinatario = %Trim(Pdestinatario) + %Trim(NTEEML) + '|';                      
                 EndIf;                                                                             
                 Reade (PCIA:'R02') VTMNTEF;                                                        
               EndDo;                                                                               
                                                                                                    
               // Valida que como minimo exista un destinatario                                     
               If Pdestinatario <>  *Blanks;                                                        
                                                                                                    
                 // Asigna compañia                                                                 
                 Pcia= PCIA;                                                                        
                                                                                                    
                 // Asigna Respuesta                                                                
                 Pres= *Blanks;                                                                     
                                                                                                    
                 // Asigna Remitente                                                                
                 Premitente = 'servicioalcliente@leonisa.com';                                      
                                                                                                    
                 // Asigna Descripcion mensaje                                                      
                 PRemMostrar= 'Pagos ' + %Trim(Htexto) +                                            
                              's por realizar en efectivo';                                         
                                                                                                    
                 // Asigna asunto                                                                   
                 Pasunto    = 'Pagos en efectivo';                                                  
                                                                                                    
                 // Asigna asunto                                                                   
                 PmensajeE  = 'Se ha generado el archivo plano con el ' +                           
                              'detalle de los pagos a realizar a ' +                                
                              'las ' + %Trim(Htexto) + 'es ' +                                      
                              'por pago en efectivo, el archivo ' +                                 
                              'se coloco en la ruta ' +                                             
                              ':/Pagos Electronicos/GNBanPag ' +                                    
                              'y el nombre del archivo es '+ %Trim(ParchivoDes);                    
                                                                                                    
                 PReplyto   = *Blanks;                                                              
                 Pcc        = *Blanks;                                                              
                 Pcco       = *Blanks;                                                              
                 Soapr      = *Blanks;                                                              
                                                                                                    
                  EnviaCorreo(Pcia            :                                                     
                             Pres            :                                                      
                             Premitente      :                                                      
                             Pdestinatario   :                                                      
                             PRemMostrar     :                                                      
                             PAsunto         :                                                      
                             PmensajeE       :                                                      
                             PReplyto        :                                                      
                             Pcc             :                                                      
                             Pcco            :                                                      
                             Soapr            );                                                    
                                                                                                    
               Endif; // Fin validación destinatario                                                
                                                                                                    
       Exfmt RDATO6;                                                                                
                                                                                                    
      /End-Free                                                                                     
     PPrFTP            E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Graba Registro de Pagos Pendientes Guías Inact //                       
       // ivas                                                             //                       
       //------------------------------------------------------------------//                       
     PPrVtwpgif        B                                                                            
     DPrVtwpgif        Pi                                                                           
      /Free                                                                                         
       Chain(n) Icedula Lmllst01;                                                                   
       If %Found(Lmllst01);                                                                         
        If Mldele = 'D' Or Mldm01 = 'X';                                                            
         Chain (Pcia:Wcbced:xSpgcam:Pzona) Vtwpgif;                                                 
         If Not %Found(Vtwpgif);                                                                    
          Pgicia = Pcia;                                                                            
          Pgicdg = Wcbced;                                                                          
          Pgicmp = xSpgcam;                                                                         
          Pgizng = Pzona;                                                                           
          Pgivlr = Wcbval;                                                                          
          If %Len(%Trim(HtextoC)) <= *Zeros;                                                        
           Pgides = 'La ' + %Trim(Htexto) + ' esta Inactiva como Compradora';                       
          Else;                                                                                     
           Pgides = 'La ' + %Trim(Htexto) + ' esta Inactiva como ' +                                
                    %Trim(HtextoC);                                                                 
          Endif;                                                                                    
          Pgiipa = *Blanks;                                                                         
          Write Regpgi;                                                                             
         Endif;                                                                                     
        Endif;                                                                                      
       Endif;                                                                                       
      /End-Free                                                                                     
     PPrVtwpgif        E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Valida que la Guía halla Enviado el RUT y que  //                       
       // este Bueno para Poder Realizar el Pago a la Guía                 //                       
       //------------------------------------------------------------------//                       
     PPrVerificarRutGuia...                                                                         
     P                 B                                                                            
     DPrVerificarRutGuia...                                                                         
     D                 Pi              n                                                            
      ** Definición de Variables de Trabajo                                                         
     DNro              s              3  0  Inz(*Zeros)                                             
     DUnro             s              3  0  Inz(*Zeros)                                             
     DPPsw             s               n    Inz(*On)                                                
     dOrespuestaPgoRUT...                                                                           
     d                 s              1a    Inz('N')                                                
     dOrespuestaIngCnt...                                                                           
     d                 s              1a    Inz('N')                                                
     dSpgcamiW         s              4a    Inz(*Blanks)                                            
     dSector           s              1a    Inz(*Blanks)                                            
     dGldzon           s              4a    Inz(*Blanks)                                            
     DNroP             s              3  0  Inz(*Zeros)                                             
      /Free                                                                                         
        SpgcamiW = *Blanks;                                                                         
        Sector = *Blanks;                                                                           
        Exec Sql                                                                                    
         Select Gldcin, Gldsct, Gldzon Into :SpgcamiW, :Sector, :Gldzon                             
         From Votrea00/Vtmgldf                                                                      
         Where Trim(Char(Gldcte)) = :xSpgced                                                        
               And Gldcia = :Pcia                                                                   
               And (Gldest = ' ' or Gldcdt >= :xSpgcam)                                             
         Order By Rrn(Vtmgldf) Desc                                                                 
         Fetch First 1 Row Only;                                                                    
                                                                                                    
        If SpgcamiW = *Blanks;                                                                      
         SpgcamiW = Spgcami;                                                                        
        Endif;                                                                                      
                                                                                                    
       PCamE = xSpgcam;                                                                             
       PCamR = *Blanks;                                                                             
       POper = '+';                                                                                 
       Pnume = '1';                                                                                 
       ProCampa(Pcia:PCamE:PCamR:POper:Pnume);                                                      
                                                                                                    
       XXpsw = 'N';                                                                                 
       Ycia = Pcia;                                                                                 
       Tced = %Trim(%Editc(xSpgced:'Z'));                                                           
       Tcns = *Blanks;                                                                              
       Conversion(Ycia:Tced:Tcns);                                                                  
       PgmPoliticasRUT(Pcia:Gldzon:Tced:OrespuestaPgoRUT:OrespuestaIngCnt);                         
       // No Aplica Política Nuevo Plan de Guías                                                    
       If Orespuesta = 'N';                                                                         
        If Pcia <= '002';                                                                           
        Exec Sql                                                                                    
         Select Count(0) Into :Nro                                                                  
         From Votrea00/Vtmrtgf                                                                      
         Where Trim(Char(Rtgidt)) = Trim(:Tced)                                                     
                And Rtgcia = :Pcia                                                                  
                And (Rtgest = ' ' Or (Rtgest = 'D' And (Rtgcdt =                                    
                Trim(:xSpgcam) Or Rtgcdt = Trim(:xSpgcam)                                           
                Or Rtgcdt = Trim(:PCamR) Or Rtgcdt >= Trim(:PCamR))));                              
                                                                                                    
        If Nro > *Zeros;                                                                            
         PPsw = *Off;                                                                               
        Else;                                                                                       
       //Exec Sql                                                                                   
       // Select Count(0) Into :Nro                                                                 
       // From Votrea00/Vtmgldf                                                                     
       // Where Trim(Char(Gldcte)) = Trim(:Tcns)                                                    
       //        And Gldcia = :Pcia                                                                 
       //        And Gldest = 'D' And Gldcdt >= :xSpgcam;                                           
         If Nro > *Zeros;                                                                           
          PPsw = *Off;                                                                              
         Endif;                                                                                     
        Endif;                                                                                      
        Else;                                                                                       
         PPsw = *Off;                                                                               
        Endif;                                                                                      
       Endif;                                                                                       
                                                                                                    
       // Si Aplica Política Nuevo Plan de Guías                                                    
       If Orespuesta = 'S';                                                                         
        If OrespuestaPgoRUT = 'S';                                                                  
         If Pcia <= '002';                                                                          
         Ycia = Pcia;                                                                               
         Tced = %Trim(%Editc(xSpgced:'Z'));                                                         
         Tcns = *Blanks;                                                                            
         Conversion(Ycia:Tced:Tcns);                                                                
                                                                                                    
         Exec Sql                                                                                   
          Select Count(0) Into :Nro                                                                 
          From Votrea00/Vtmrtgf                                                                     
          Where Trim(Char(Rtgidt)) = Trim(:Tced)                                                    
                And Rtgcia = :Pcia                                                                  
                And (Rtgest = ' ' Or (Rtgest = 'D' And (Rtgcdt =                                    
                Trim(:xSpgcam) Or Rtgcdt = Trim(:xSpgcam)                                           
                Or Rtgcdt = Trim(:PCamR)                                                            
                Or Rtgcdt >= Trim(:PCamR))));                                                       
                                                                                                    
         If Nro > *Zeros;                                                                           
          PPsw = *Off;                                                                              
         Else;                                                                                      
        //Exec Sql                                                                                  
        // Select Count(0) Into :Nro                                                                
        // From Votrea00/Vtmgldf                                                                    
        // Where Trim(Char(Gldcte)) = Trim(:Tcns)                                                   
        //        And Gldcia = :Pcia                                                                
        //        And Gldest = 'D' And Gldcdt >= :xSpgcam;                                          
          If Nro > *Zeros;                                                                          
           PPsw = *Off;                                                                             
          Endif;                                                                                    
         Endif;                                                                                     
         Else;                                                                                      
          PPsw = *Off;                                                                              
         Endif;                                                                                     
        Else;                                                                                       
         PPsw = *Off;                                                                               
        Endif;                                                                                      
       Endif;                                                                                       
                                                                                                    
       If PPsw = *Off;                                                                              
        PrVtwpcgf(Pcia:Gldzon:xSpgcam:xSpgced:Sector);                                              
        // Verifica si la Guía Cumplio con el Número de Inscripciones                               
        // requeridas                                                                               
        Exec Sql                                                                                    
         Select Count(0) Into :Unro                                                                 
         From Votrea00/Vttdngf                                                                      
         Where Dngcia = :Pcia And                                                                   
               Dngzng = :Gldzon And                                                                 
         //    Dngscg = :Sector And                                                                 
               Dngcdg = :xSpgced And                                                                
               Dngcmp = :xSpgcam And                                                                
               Dngcdr = 5 And                                                                       
               Dngest <> 'A';                                                                       
        If Unro > *Zeros;                                                                           
         Unro = *Zeros;                                                                             
         Exec Sql                                                                                   
          Select Count(0) Into :Unro                                                                
          From Votrea00/Vtmspgf                                                                     
          Where Spgcia = :Pcia And                                                                  
                Spgzon = :Gldzon And                                                                
          //    Spgsec = :Sector And                                                                
                Spgced = :xSpgced And                                                               
                Spgcam >= :SpgcamiW And                                                             
                Spginp = ' ';                                                                       
         If Unro <= *Zeros;                                                                         
          PPsw = *On;                                                                               
          XXpsw = 'S';                                                                              
         Endif;                                                                                     
        Endif;                                                                                      
       Endif;                                                                                       
                                                                                                    
       If PPsw = *Off;                                                                              
        PrVtwpcgf(Pcia:Gldzon:xSpgcam:xSpgced:Sector);                                              
        NroP = *Zeros;                                                                              
        Exec Sql                                                                                    
         Select Count(0) Into :NroP                                                                 
         From Votrea00/Vttdngf                                                                      
         Where Dngcia = :Pcia And                                                                   
               Dngzng = :Gldzon And                                                                 
          //   Dngscg = :Sector And                                                                 
               Dngcdg = :xSpgced And                                                                
               Dngcmp = :xSpgcam And                                                                
               Dngcdr <> 5 And                                                                      
               Dngest <> 'A';                                                                       
         If NroP > *Zeros;                                                                          
          PPsw = *On;                                                                               
          XXpsw = 'S';                                                                              
         Endif;                                                                                     
       Endif;                                                                                       
                                                                                                    
       Return PPsw;                                                                                 
      /End-Free                                                                                     
     PPrVerificarRutGuia...                                                                         
     P                 E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Consulta en el Sistema si Hay Pagos Pendientes //                       
       // de las Guías Con Estado E que es RUT no Enviado                  //                       
       //------------------------------------------------------------------//                       
     PPrVerificaSiHayPagosenEstadoE...                                                              
     P                 B                                                                            
     DPrVerificaSiHayPagosenEstadoE...                                                              
     D                 Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DSpgciai          s              3a   Inz(*Blanks)                                             
     DSpgseci          s              1a   Inz(*Blanks)                                             
     DSpgtrri          s              1a   Inz(*Blanks)                                             
     DSpgcedi          s              9  0 Inz(*Zeros)                                              
     DWregion          s             30a   Inz(*Blanks)                                             
     DSpgcediAux       s              9  0 Inz(*Blanks)                                             
     DWsector          s              3a   Inz(*Blanks)                                             
     DHnum             s              9  0 Inz(*Blanks)                                             
     DKnum             s              9  0 Inz(*Blanks)                                             
      /Free                                                                                         
       Onum = *Zeros;                                                                               
       Spgzoni = *Blanks;                                                                           
       Spgcami = *Blanks;                                                                           
       ProcesarCasosMarcadosEnError();                                                              
       Exec Sql                                                                                     
        Declare CurRut Cursor For                                                                   
        Select Spgcia, Spgzon, Spgsec, Spgtrr, Spgced, Spgcam                                       
        From Votrea00/Vtmspgf                                                                       
        Where Spgcia = :Pcia And Spginp = 'E' And                                                   
              Spgcam >= '1615' And Spgvps > 0                                                       
        Order By Spgcia, Spgced ,Spgcam Desc;                                                       
                                                                                                    
       Exec Sql                                                                                     
        Open CurRut;                                                                                
                                                                                                    
       Exec Sql                                                                                     
        Fetch CurRut Into :Spgciai, :Spgzoni, :Spgseci, :Spgtrri, :Spgcedi,                         
                          :Spgcami;                                                                 
                                                                                                    
       Dow SqlCod <> 100 And SqlCod >= *Zeros;                                                      
        Knum = 1;                                                                                   
        If Spgcedi <> SpgcediAux And Hnum <= Onum;                                                  
         Hnum = *Zeros;                                                                             
         Wregion = PrRegion();                                                                      
         Onum = *Zeros;                                                                             
         Wsector = Spgseci;                                                                         
         PgmNroCamRut(Pcia:Wregion:Spgzoni:Wsector:Spgcami:Onum);                                   
         SpgcediAux = Spgcedi;                                                                      
         Onumx = Onum;                                                                              
        Endif;                                                                                      
        If PrValidarCampa() = *On;                                                                  
         xSpgced = Spgcedi;                                                                         
         Orespuesta = *Blanks;                                                                      
         Pccte = %Trim(%Editc(Spgcedi:'Z'));                                                        
         Icaso = *Blanks;                                                                           
         Ifecha = *Blanks;                                                                          
         PgmNewPlanG(Spgciai:Spgzoni:Icaso:Pccte:Ifecha:Orespuesta);                                
         xSpgcam = Spgcami;                                                                         
         xSpgced = Spgcedi;                                                                         
         If PrVerificarRutGuia() = *Off;                                                            
          Hnum = 1;                                                                                 
          If Knum <= Onumx;                                                                         
          //Pago en Efectivo                                                                        
          Exec Sql                                                                                  
           Update Votrea00/Vtmspgf Set Spginp = ' ', Spgccr = 042                                   
           Where Spgcia = :Pcia And                                                                 
                 Spgzon = :Spgzoni And                                                              
                 Spgsec = :Wsector And                                                              
                 Spgtrr = :Ptr1 And                                                                 
                 Spgced = :xSpgced And                                                              
                 Spgcam = :Spgcami And                                                              
                 Spginp = 'E' And                                                                   
                 Spgccr = 043;                                                                      
          If SqlCod = -000000803;                                                                   
           Clear Evalor;                                                                            
           Exec Sql                                                                                 
            Select Spgvps Into :Evalor                                                              
            From Votrea00/Vtmspgf                                                                   
            Where Spgcia = :Pcia And                                                                
                  Spgzon = :Spgzoni And                                                             
                  Spgsec = :Wsector And                                                             
                  Spgtrr = :Ptr1 And                                                                
                  Spgced = :xSpgced And                                                             
                  Spgcam = :Spgcami And                                                             
                  Spginp = 'E' And                                                                  
                  Spgccr = 043;                                                                     
                                                                                                    
          //Pago del Dinero por Ajuste a la Cartera de la Compradora                                
          Exec Sql                                                                                  
           Update Votrea00/Vtmspgf Set Spginp = ' ',                                                
                                       Spgvds = Spgvds + :Evalor,                                   
                                       Spgvps = Spgvps + :Evalor,                                   
                                       Spgnbt = ' ',                                                
                                       Spgfbt = ' '                                                 
           Where Spgcia = :Pcia And                                                                 
                 Spgzon = :Spgzoni And                                                              
                 Spgsec = :Wsector And                                                              
                 Spgtrr = :Ptr1 And                                                                 
                 Spgced = :xSpgced And                                                              
                 Spgcam = :Spgcami And                                                              
                 Spgccr = 042;                                                                      
                                                                                                    
           Exec Sql                                                                                 
            Update Votrea00/Vtwlpgf Set Lpgvde = 0, Lpgfde = ' '                                    
            Where Lpgcia = :Pcia And                                                                
                  Lpgcdg = :xSpgced And                                                             
                  Lpgcmp = :Spgcami;                                                                
                                                                                                    
          Exec Sql                                                                                  
           Delete From Votrea00/Vtmspgf                                                             
           Where Spgcia = :Pcia And                                                                 
                 Spgzon = :Spgzoni And                                                              
                 Spgsec = :Wsector And                                                              
                 Spgtrr = :Ptr1 And                                                                 
                 Spgced = :xSpgced And                                                              
                 Spgcam = :Spgcami And                                                              
                 Spginp = 'E' And                                                                   
                 Spgccr = 043;                                                                      
          Endif;                                                                                    
                                                                                                    
          //Pago del Dinero por Ajuste a la Cartera de la Compradora                                
          Exec Sql                                                                                  
           Update Votrea00/Vtmspgf Set Spginp = ' '                                                 
           Where Spgcia = :Pcia And                                                                 
                 Spgzon = :Spgzoni And                                                              
                 Spgsec = :Wsector And                                                              
                 Spgtrr = :Ptr1 And                                                                 
                 Spgced = :xSpgced And                                                              
                 Spgcam = :Spgcami And                                                              
                 Spginp = 'E' And                                                                   
                 Spgccr = 042;                                                                      
                                                                                                    
          Exec Sql                                                                                  
           Update Votrea00/Vttpprf Set Pprest = 'A',                                                
                  Ppraut = 'RUT INGRESO CORRECTAMENTE, SE RECIBIO NOTIFICACION'                     
           Where Pprcia = :Pcia And                                                                 
                 Pprzna = :Spgzoni And                                                              
                 Pprsec = :Wsector And                                                              
                 Pprcdg = :xSpgced And                                                              
                 Pprcmp = :Spgcami And                                                              
                 Pprest = 'E';                                                                      
          Knum += 1;                                                                                
         Endif;                                                                                     
        Endif;                                                                                      
        Endif;                                                                                      
        Exec Sql                                                                                    
         Fetch CurRut Into :Spgciai, :Spgzoni, :Spgseci, :Spgtrri, :Spgcedi,                        
                           :Spgcami;                                                                
       Enddo;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close CurRut;                                                                               
      /End-Free                                                                                     
     PPrVerificaSiHayPagosenEstadoE...                                                              
     P                 E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Graba los Casos Pendientes Pago Guía por Falta //                       
       // del RUT                                                          //                       
       //------------------------------------------------------------------//                       
     PPrPagosPendientesGuias...                                                                     
     P                 B                                                                            
     DPrPagosPendientesGuias...                                                                     
     D                 Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DTvalor           s             11  2 Inz(*Zeros)                                              
     dOrespuestaPgoRUT...                                                                           
     d                 s              1a    Inz('N')                                                
     dOrespuestaIngCnt...                                                                           
     d                 s              1a    Inz('N')                                                
      /Free                                                                                         
       If Spgced = 0;                                                                               
       Spgcia = xSpgcia;                                                                            
       Spgzon = xSpgzon;                                                                            
       Spgcam = xSpgcam;                                                                            
       Spgsec = xSpgsec;                                                                            
       Spgtrr = xSpgtrr;                                                                            
       Spgced = xSpgced;                                                                            
       Endif;                                                                                       
       Tcns = %Trim(%Editc(Spgced:'Z'));                                                            
       Tced = *Blanks;                                                                              
       Conversion(Spgcia:Tced:Tcns);                                                                
       PgmPoliticasRUT(Ycia:Spgzon:Tced:OrespuestaPgoRUT:OrespuestaIngCnt);                         
       // Si Aplica Política Nuevo Plan de Guías                                                    
       If OrespuestaPgoRUT = 'S';                                                                   
        If XXpsw = 'N';                                                                             
         Clear *Nokey Regppr;                                                                       
         chain (Spgcia:Spgcam:Spgzon:Spgsec:Spgced) Vttpprf;                                        
         if not %found(Vttpprf);                                                                    
           Pprcia= Spgcia;                                                                          
           Pprcmp= Spgcam;                                                                          
           Pprzna= Spgzon;                                                                          
           Pprsec= Spgsec;                                                                          
           Pprcdg= Spgced;                                                                          
                                                                                                    
           Tvalor = *Zeros;                                                                         
           Exec Sql                                                                                 
            Select Sum(Spgvps) Into :Tvalor                                                         
            From Votrea00/Vtmspgf                                                                   
            Where Spgcia = :Spgcia And                                                              
                  Spgzon = :Spgzon And                                                              
                  Spgcam = :Spgcam And                                                              
                  Spgced = :Spgced And                                                              
                  Spgsec = :Spgsec;                                                                 
                                                                                                    
           Pprvlr = Tvalor;                                                                         
           Pprest = 'E';                                                                            
           Pprmot = 'Descuento NO Realizado RUT no Enviado';                                        
          Pprdes = 'Pago NO realizado a la ' + %Trim(Htexto) +                                      
                   ' en C' + %Trim(Spgcam) +                                                        
                   ' porque no se tiene la información del RUT.';                                   
           Pprfec= %char(%date());                                                                  
           Pprhor= %char(%time():*hms);                                                             
           Pprusr= User;                                                                            
           Write Regppr;                                                                            
         Endif;                                                                                     
        Endif;                                                                                      
       Endif;                                                                                       
      /End-Free                                                                                     
     PPrPagosPendientesGuias...                                                                     
     P                 E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Valida los Pagos si es por Ajuste en Cartera o //                       
       // es en Efectivo                                                   //                       
       //------------------------------------------------------------------//                       
     PPrValidarPagos   B                                                                            
     DPrValidarPagos   Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DSpgciaJ          s              3     Inz(*Blanks)                                            
     DSpgzonJ          s              3     Inz(*Blanks)                                            
     DSpgcamJ          s              4     Inz(*Blanks)                                            
     DSpgcedJ          s              9  0  Inz(*Zeros)                                             
     DSpgvpsJ          s             11  2  Inz(*Zeros)                                             
     DSpgsecJ          s              1     Inz(*Blanks)                                            
     DSpgtrrJ          s              1     Inz(*Blanks)                                            
     DValorPgE         s             11  2  Inz(*Zeros)                                             
     DIestado          s              1     Inz(*Blanks)                                            
      /Free                                                                                         
       Exec Sql                                                                                     
        Declare CurJ Cursor For                                                                     
        Select Distinct Spgcia, Spgzon, Spgcam, Spgced, Spgvps,                                     
                        Spgsec, Spgtrr                                                              
        From Votrea00/Vtmspgf                                                                       
        Where Spgnbt = ' ' And Spgcia = :Pcia                                                       
        And Spgvps <> '0' And Spginp = ' ';                                                         
                                                                                                    
       Exec Sql                                                                                     
        Open CurJ;                                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Fetch CurJ Into :SpgciaJ, :SpgzonJ, :SpgcamJ, :SpgcedJ, :SpgvpsJ,                           
                        :SpgsecJ, :SpgtrrJ;                                                         
                                                                                                    
       Dow SqlCod <> 100 And SqlCod >= *Zeros;                                                      
        ValorPgE = *Zeros;                                                                          
        Exec Sql                                                                                    
         Select Cgevlr Into :ValorPgE                                                               
         From Votrea00/Vtmcgef                                                                      
         Where Cgecia = :SpgciaJ And Cgecmp = :SpgcamJ                                              
         Fetch First 1 Row Only;                                                                    
                                                                                                    
        If SpgvpsJ >= ValorPgE And ValorPgE <> *Zeros;                                              
         Iestado = *Blanks;                                                                         
         Exec Sql                                                                                   
          Select Zletpr Into :Iestado                                                               
          From Votrea00/Vttzlef                                                                     
          Where Zleccg = :SpgciaJ And                                                               
                ZleCdg = :SpgcedJ And                                                               
                Zlecmp = :SpgcamJ                                                                   
          Order By Rrn(Vttzlef) Desc;                                                               
         If Iestado = 'T';                                                                          
          Exec Sql                                                                                  
           Update Vtmspgf Set Spginp = 'T'                                                          
           Where Spgcia = :SpgciaJ And                                                              
                 Spgzon = :Pzona And                                                                
                 Spgcam = :SpgcamJ And                                                              
                 Spgsec = :SpgsecJ And                                                              
                 Spgtrr = :SpgtrrJ And                                                              
                 Spgced = :SpgcedJ;                                                                 
         Endif;                                                                                     
        Endif;                                                                                      
        Exec Sql                                                                                    
         Fetch CurJ Into :SpgciaJ, :SpgzonJ, :SpgcamJ, :SpgcedJ, :SpgvpsJ,                          
                         :SpgsecJ, :SpgtrrJ;                                                        
       Enddo;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close CurJ;                                                                                 
                                                                                                    
      /End-Free                                                                                     
     PPrValidarPagos   E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Consulta la Región en la que esta la Zona      //                       
       //------------------------------------------------------------------//                       
     PPrRegion         B                                                                            
     DPrRegion         Pi            12a                                                            
     ** Definiciòn de Variables de Trabajo                                                          
     DRegion           s             12a   Inz(*Blanks)                                             
     DSqlStmt          s           1000a   Inz(*Blanks)                                             
      /Free                                                                                         
        Region = *Blanks;                                                                           
        SqlStmt = *Blanks;                                                                          
        SqlStmt = 'Select Sminf2 ' +                                                                
                  'From Opf' + Pcia + '/Pslman ' +                                                  
                  'Where Trim(Smslsp) = ' + '''' + Spgzoni + '''' + ' And ' +                       
                  'Smdele = '' ''' + ' ' +                                                          
                  'Fetch First 1 Row Only';                                                         
                                                                                                    
        Exec Sql                                                                                    
         Prepare CurB From :SqlStmt;                                                                
                                                                                                    
        Exec Sql                                                                                    
         Declare CursorB Cursor For CurB;                                                           
                                                                                                    
        Exec Sql                                                                                    
         Open CursorB;                                                                              
                                                                                                    
        Exec Sql                                                                                    
         Fetch CursorB Into :Region;                                                                
                                                                                                    
        If Sqlcod = 100 Or Sqlcod < *Zeros;                                                         
         Region = *Blanks;                                                                          
        Endif;                                                                                      
                                                                                                    
        Exec Sql                                                                                    
         Close CursorB;                                                                             
                                                                                                    
        Return Region;                                                                              
      /End-Free                                                                                     
     PPrRegion         E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Consulta si la Guía Tiene Cuenta Activa        //                       
       //------------------------------------------------------------------//                       
     PPrVtmipgf        B                                                                            
     DPrVtmipgf        Pi              n                                                            
     DHcedula                        30a                                                            
     ** Definiciòn de Variables de Trabajo                                                          
     DPsw              s               n   Inz(*Off)                                                
     DKnro             s              4  0 Inz(*Zeros)                                              
      /Free                                                                                         
       Ycia = Pcia;                                                                                 
       Tced = %Trim(Hcedula);                                                                       
       Tcns = *Blanks;                                                                              
       Conversion(Ycia:Tced:Tcns);                                                                  
       Clear Knro;                                                                                  
       Exec Sql                                                                                     
        Select Count(0) Into :Knro                                                                  
        From Votrea00/Vtmipgf                                                                       
        Where Ipgcia = :Pcia And                                                                    
              (Trim(Char(Ipgcgc)) = Trim(:Tced) Or                                                  
               Trim(Char(Ipgcgc)) = Trim(:Tcns))                                                    
               And IpgEst = ' ';                                                                    
                                                                                                    
       If Knro > 0;                                                                                 
        Psw = *On;                                                                                  
       Else;                                                                                        
        Clear Knro;                                                                                 
        Exec Sql                                                                                    
         Select Count(0) Into :Knro                                                                 
         From Votrea00/Vtmgldf                                                                      
         Where Gldcia = :Pcia And Gldest <> ' ' And                                                 
               Gldcdt = :PCamEE;                                                                    
        If Knro > 0;                                                                                
         Psw = *On;                                                                                 
        Endif;                                                                                      
       Endif;                                                                                       
        Return Psw;                                                                                 
      /End-Free                                                                                     
     PPrVtmipgf        E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Consulta si la Guía Tiene Pagos Retenidos por  //                       
       // el No Envio del RUT pero que sean Consecutivos                   //                       
       //------------------------------------------------------------------//                       
     PPrValidarCampa   B                                                                            
     DPrValidarCampa   Pi              n                                                            
     ** Definiciòn de Variables de Trabajo                                                          
     DPsw              s               n   Inz(*Off)                                                
     DTnro             s              2  0 Inz(1)                                                   
     DOnro             s              2  0 Inz(1)                                                   
     DIdx              s              9  0 Inz(0)                                                   
     DWcampa           s              4a   Inz(*Blanks)                                             
      /Free                                                                                         
       Pzona = Spgzoni;                                                                             
       Exec Sql                                                                                     
        Select Estcam Into :Wcampa                                                                  
        From Orderp00/Opiestf1                                                                      
        Where Estcia = :Pcia And Estzon = :Pzona And                                                
              Estest = 'S'                                                                          
        Order By Rrn(Opiestf1) Desc                                                                 
        Fetch First 1 Row Only;                                                                     
                                                                                                    
       For Idx = 1 To Onum;                                                                         
        Onro = *Zeros;                                                                              
        Exec Sql                                                                                    
         Select Count(0) Into :Onro                                                                 
         From Votrea00/Vtmspgf                                                                      
         Where Spgcia = :Pcia And                                                                   
               Spgcam = :Wcampa And                                                                 
               Spgzon = :Pzona And                                                                  
               Spginp = 'E' And                                                                     
               Spgvps > 0;                                                                          
                                                                                                    
         If Onro > *Zeros;                                                                          
          Tnro += 1;                                                                                
         Endif;                                                                                     
         PCamE = Wcampa;                                                                            
         PCamR = *Blanks;                                                                           
         POper = '-';                                                                               
         Pnume = '1';                                                                               
         ProCampa(Pcia:PCamE:PCamR:POper:Pnume);                                                    
         Wcampa = PCamR;                                                                            
       EndFor;                                                                                      
                                                                                                    
       If Tnro > *Zeros;                                                                            
        Psw = *On;                                                                                  
       Endif;                                                                                       
                                                                                                    
       Onumx = Tnro;                                                                                
       Return Psw;                                                                                  
      /End-Free                                                                                     
     PPrValidarCampa   E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       //Procedimiento que Graba Información en el Archivo Vtwlpgf         //                       
       //------------------------------------------------------------------//                       
     PPrVtwlpgf        B                                                                            
     DPrVtwlpgf        Pi                                                                           
     ** Definiciòn de Variables de Trabajo                                                          
     DW_Condtl         s              9  0                                                          
     DSqlCodAux        s              9  0                                                          
     Dw_VlrDescuento   s             15  2                                                          
     DHnro             s              9  0                                                          
     DHvalor           s             11  2                                                          
     DInro             s              9  0                                                          
     DIvalor           s             11  2                                                          
     DKnro             s              9  0                                                          
      /Free                                                                                         
                                                                                                    
       Clear Hnro;                                                                                  
       Clear Hvalor;                                                                                
       Exec Sql                                                                                     
       Select Count(0), Max(Cpgvlr) Into :Hnro, :Hvalor                                             
       From Votrea00/Vttcpgf                                                                        
       Where Cpgcia = :XSpgcia And                                                                  
             Cpgcdg = :XSpgced And                                                                  
             Cpgzon = :XSpgzon And                                                                  
             Cpgsec = :XSpgsec And                                                                  
             Cpgcmp = :XSpgcam And                                                                  
             Cpgcpc = 043;                                                                          
                                                                                                    
       Clear Inro;                                                                                  
       Clear Ivalor;                                                                                
       Exec Sql                                                                                     
       Select Count(0), Max(Spgvds) Into :Inro, :Ivalor                                             
       From Votrea00/Vtmspgf                                                                        
       Where Spgcia = :XSpgcia And                                                                  
             Spgced = :XSpgced And                                                                  
             Spgzon = :XSpgzon And                                                                  
             Spgsec = :XSpgsec And                                                                  
             Spgcmp = :XSpgcam And                                                                  
             Spgccr = 042;                                                                          
                                                                                                    
       Clear Knro;                                                                                  
       Exec Sql                                                                                     
       Select Count(0) Into :Knro                                                                   
       From Votrea00/Vtmspgf                                                                        
       Where Spgcia = :XSpgcia And                                                                  
             Spgced = :XSpgced And                                                                  
             Spgzon = :XSpgzon And                                                                  
             Spgsec = :XSpgsec And                                                                  
             Spgcmp = :XSpgcam;                                                                     
                                                                                                    
       If Hvalor = Ivalor And Hnro = Inro And Knro = 1 And Ivalor > 0;                              
        Exec Sql                                                                                    
         Update Votrea00/Vtwlpgf set Lpgtds = Lpgvdc ,                                              
                                     Lpgvde = 0,                                                    
                                     Lpgfde = ' '                                                   
          Where LPgCia= :XSpgcia And                                                                
                LPgCmp= :XSpgcam And                                                                
                LPgZna= :XSpgzon And                                                                
                LPgSec= :XSpgsec And                                                                
                LPgCdg= :XSpgced;                                                                   
       Endif;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Select MLNAME Into :qLpgNom                                                                 
        From LMLLST01                                                                               
        Where MLCSTÑ = :QLpgCdg;                                                                    
       If Qlpgvde < *Zeros;                                                                         
          Qlpgvde = Qlpgvde * (-1);                                                                 
       Elseif Qlpgvde = *Zeros;                                                                     
          clear qLPGFDE;                                                                            
       EndIf;                                                                                       
       If QLpgVdc < *Zeros;                                                                         
          QLpgVdc = QLpgVdc * (-1);                                                                 
       ElseIf QLpgVdc = *Zeros;                                                                     
          Clear QLpgFdc;                                                                            
       EndIf;                                                                                       
       // Recupera Valor para campaña de posteo                                                     
       PrRecuperaPosteo();                                                                          
       Clear w_VlrDescuento;                                                                        
       Exec Sql                                                                                     
        Select Sum(Spgvds) Into :w_VlrDescuento                                                     
        From Vtmspgf                                                                                
        Where Spgcia =:XSpgcia                                                                      
          And Spgzon =:XSpgzon                                                                      
          And Spgcam =:XSpgcam                                                                      
          And Spgsec =:XSpgsec                                                                      
          And Spgtrr =:XSpgtrr                                                                      
          And Spgced =:XSpgced;                                                                     
       // Verifica que el vaLor de pago en cartera sea igual al valor de posteo en cartera          
       W_Condtl = w_VlrDescuento - w_AcuDtl;                                                        
       If W_Condtl = *Zeros;                                                                        
          QLpgDsa = 'SI';                                                                           
       Else;                                                                                        
          QLpgDsa ='NO';                                                                            
       EndIf;                                                                                       
       Qlpgtds = qlpgvde + qlpgvdc;                                                                 
       Qlpgvrc = qlpgvdc;                                                                           
       Qlpgfvr = qlpgfdc;                                                                           
       Clear Fields5;                                                                               
       Exec Sql                                                                                     
       Select Lpgvde, Lpgvdc, Lpgfde, Lpgfdc                                                        
             into :Ylpgvde,:Ylpgvdc,:YLpgfde,:YLpgfdc                                               
       From Votrea00/Vtwlpgf                                                                        
       Where LPgCia= :QLPgCia And                                                                   
             LPgCmp= :QLPgCmp And                                                                   
             LPgReg= :QLPgReg And                                                                   
             LPgZna= :QLPgZna And                                                                   
             LPgSec= :QLPgSec And                                                                   
             LPgCdg= :QLPgCdg And                                                                   
             LpgIpt= :QLpgIpt;                                                                      
       SqlCodAux = SqlCod;                                                                          
       If Wcbcct = 42; // Cartera                                                                   
          Qlpgfdc = XSpgfbt;                                                                        
          Qlpgvdc = Qlpgvdc;                                                                        
          Clear TnroP;                                                                              
          Exec Sql                                                                                  
           Select Count(0) Into :TnroP                                                              
           From Votrea00/Vtmspgf                                                                    
           Where SpgCia= :QLPgCia And                                                               
                 Spgcam= :QLPgCmp And                                                               
                 Spgzon= :QLPgZna And                                                               
                 Spgsec= :QLPgSec And                                                               
                 SpgCed= :QLPgCdg And                                                               
                 Spgccr = 43;                                                                       
          If TnroP > 0;                                                                             
           Qlpgfde = Ylpgfde;                                                                       
           Qlpgvde = Ylpgvde;                                                                       
          Endif;                                                                                    
       Else; //Wcbcct = 43; // Efectivo                                                             
          Qlpgvde = Qlpgvde;                                                                        
          Qlpgfde = XSpgfbt;                                                                        
                                                                                                    
          Clear TnroP;                                                                              
          Exec Sql                                                                                  
           Select Count(0) Into :TnroP                                                              
           From Votrea00/Vtmspgf                                                                    
           Where SpgCia= :QLPgCia And                                                               
                 Spgcam= :QLPgCmp And                                                               
                 Spgzon= :QLPgZna And                                                               
                 Spgsec= :QLPgSec And                                                               
                 SpgCed= :QLPgCdg And                                                               
                 Spgccr = 42;                                                                       
          If TnroP > 0;                                                                             
           Qlpgvdc = YLpgvdc;                                                                       
           Qlpgfdc = YLpgfdc;                                                                       
          Endif;                                                                                    
       EndIf;                                                                                       
       If Qlpgvde = Qlpgvdc;                                                                        
          Clear Qlpgvdc;                                                                            
          Clear Qlpgfdc;                                                                            
       EndIf;                                                                                       
       Qlpgtds = Qlpgvde + QLpgVdc;                                                                 
       Qlpgvrc = QLpgVdc;                                                                           
       Qlpgfvr = QLpgFdc;                                                                           
       If Qlpgdsa ='NO';                                                                            
          Qlpgfde = ' ';                                                                            
          Qlpgfdc = ' ';                                                                            
          Qlpgfvr = ' ';                                                                            
          If Qlpgmot= *Blanks;                                                                      
             Qlpgmot = 'Ajuste no efectuado por cartera';                                           
          EndIf;                                                                                    
       EndIf;                                                                                       
       If SqlCodAux = *Zeros;                                                                       
          Exec Sql                                                                                  
          Update Votrea00/Vtwlpgf set LpgTds = :QLpgTds,                                            
                                      LpgVde = :Qlpgvde,                                            
                                      LpgVdc = :QLpgVdc,                                            
                                      LpgFde = :QLpgFde,                                            
                                      LpgFdc = :QLpgFdc,                                            
                                      LpgVrc = :QLpgVrc,                                            
                                      LpgFvr = :QLpgFvr,                                            
                                      LpgMot = :Qlpgmot,                                            
                                      LpgDsa = :QLpgDsa                                             
           Where LPgCia= :QLPgCia And                                                               
                 LPgCmp= :QLPgCmp And                                                               
                 LpgReg= :QLpgReg And                                                               
                 LPgZna= :QLPgZna And                                                               
                 LPgSec= :QLPgSec And                                                               
                 LPgCdg= :QLPgCdg And                                                               
                 LpgIpt= :QLpgIpt;                                                                  
       Else;                                                                                        
          Exec Sql                                                                                  
           Insert into Votrea00/Vtwlpgf values(:Fields4);                                           
       EndIf;                                                                                       
       Clear Fields4;                                                                               
      /End-Free                                                                                     
     PPrVtwlpgf        E                                                                            
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //Prototipo que recupera Valor por campaña en posteo de cartera    //                         
      //////////////////////////////////////////////////////////////////////                        
     PPrRecuperaPosteo...                                                                           
     P                 B                                                                            
     DPrRecuperaPosteo...                                                                           
     D                 Pi                                                                           
     Dw_campana        s              4A                                                            
     Dw_WADTNDÑ        s              4A                                                            
     Dw_SumaVlrRetenido...                                                                          
     D                 s             15  2                                                          
     DWtexto           s              8                                                             
      /Free                                                                                         
       //Verifica si Zona es parte de nuevo plan de Guías                                           
       w_campana=Qlpgcmp;                                                                           
       Clear w_AcuDtl;                                                                              
       Clear Fields6;                                                                               
       Exec sql                                                                                     
       Declare csr3 cursor for                                                                      
       Select *                                                                                     
       From pardtl                                                                                  
       Where AdcstÑ = :QLpgCdg;                                                                     
       Exec Sql                                                                                     
        Open csr3;                                                                                  
       Exec Sql                                                                                     
        fetch csr3 Into :Fields6;                                                                   
       Dow SqlCod = *Zeros;                                                                         
           Clear wadtndÑ;                                                                           
           WadtndÑ  = QadtndÑ;                                                                      
           PgmRecuCÑ(wadtndÑ);                                                                      
           W_wadtndÑ = %subst(wadtndÑ:5:8);                                                         
           Clear Wtexto;                                                                            
           Wtexto = %Subst(QAdtndÑ:1:8);                                                            
           Clear ooPos;                                                                             
           ooPOS = %Scan('DctoCdLP':QAdtndÑ);                                                       
           If ooPos <= 0;                                                                           
            ooPOS = %Scan('DctoCdLE':QAdtndÑ);                                                      
           Endif;                                                                                   
           If %trim(w_wadtndÑ)=%trim(w_campana) And                                                 
           ooPOS > 0;                                                                               
              If Qadtram < *zeros;                                                                  
                 Qadtram = Qadtram * -1;                                                            
                 w_acudtl  += Qadtram;                                                              
              EndIf;                                                                                
           EndIf;                                                                                   
          Exec Sql                                                                                  
            fetch csr3 Into :Fields6;                                                               
       EndDo;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close csr3;                                                                                 
       // Recupera Valor retenido a compradora                                                      
       Clear w_SumaVlrRetenido;                                                                     
       Exec Sql                                                                                     
        Select Sum(Vrfvrr) Into :w_SumaVlrRetenido                                                  
        From Vtwvrff                                                                                
        Where Vrfcia =:QLPgCia                                                                      
          And Vrfcam =:QLPgCmp                                                                      
          And Vrfzna =:QLPgZna                                                                      
          And Vrfced =:QLPgCdg;                                                                     
       If w_SumaVlrRetenido > *Zeros;                                                               
          w_acudtl +=w_SumaVlrRetenido;                                                             
       EndIf;                                                                                       
      /End-Free                                                                                     
     PPrRecuperaPosteo...                                                                           
     P                 E                                                                            
      //////////////////////////////////////////////////////////////////////                        
      //Prototipo que recupera Valor de Cartera o Efectivo                //                        
      //////////////////////////////////////////////////////////////////////                        
     PPrRecuperaPagosL...                                                                           
     P                 B                                                                            
     DPrRecuperaPagosL...                                                                           
     D                 Pi                                                                           
      /Free                                                                                         
        Clear DsNewPlanGu;                                                                          
        PgmNewPlanG(QLPgCia:QLPgZna:wcaso:wcedula:wfecha:Orespuestaw);                              
       If OrespuestaW = 'S';                                                                        
          QLpgIpt= 'S';                                                                             
       Else;                                                                                        
          QLpgIpt= 'N';                                                                             
       Endif;                                                                                       
       If Wcbcct = 042;                                                                             
          Exec Sql                                                                                  
          Select Lpgvde, Lpgfde into :Qlpgvde,:QLpgfde                                              
          From Votrea00/Vtwlpgf                                                                     
          Where LpgCia= :QLpgCia And                                                                
          LpgCmp= :QLpgCmp And                                                                      
          LpgReg= :QLpgReg And                                                                      
          LpgZna= :QLpgZna And                                                                      
          LpgSec= :QLpgSec And                                                                      
          LpgCdg= :QLpgCdg And                                                                      
          LpgIpt= :QLpgIpt;                                                                         
          If SqlCod = 100 Or SqlCod < *Zeros;                                                       
             Qlpgvde = *Zeros;                                                                      
             Qlpgfde = *Blanks;                                                                     
          Endif;                                                                                    
          If Qlpgvde = QLpgvdc;                                                                     
             Clear QLpgvdc;                                                                         
          EndIf;                                                                                    
       Else;                                                                                        
          Exec Sql                                                                                  
          Select Lpgvdc, Lpgfdc into :qLpgvdc,:qLpgfdc                                              
          From Votrea00/Vtwlpgf                                                                     
          Where LPgCia= :QLPgCia And                                                                
          LPgCmp= :QLPgCmp And                                                                      
          LPgReg= :QLPgReg And                                                                      
          LPgZna= :QLPgZna And                                                                      
          LPgSec= :QLPgSec And                                                                      
          LPgCdg= :QLPgCdg And                                                                      
          LPgIpt= :QLPgIpt;                                                                         
          If SqlCod = 100 Or SqlCod < *Zeros;                                                       
             qLpgvdc = *Zeros;                                                                      
             qLpgfdc = *Blanks;                                                                     
          Endif;                                                                                    
          If QLpgvdc = Qlpgvde;                                                                     
             Clear Qlpgvde;                                                                         
          EndIf;                                                                                    
       EndIf;                                                                                       
      /End-Free                                                                                     
     PPrRecuperaPagosL...                                                                           
     P                 E                                                                            
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //Prototipo que verifica la campaña en la cual se inactivo la guia  //                        
      //////////////////////////////////////////////////////////////////////                        
     PPrVtmgldf        B                                                                            
     DPrVtmgldf        Pi              n                                                            
     DPsw              s               n    Inz(*Off)                                               
     DKnro             s              4  0 Inz(*Zeros)                                              
      /Free                                                                                         
        Exec Sql                                                                                    
         Select Count(0) Into :Knro                                                                 
         From Votrea00/Vtmgldf                                                                      
         Where Gldcia = :Pcia And Gldest <> ' ' And                                                 
               Gldcdt = :PCamEE;                                                                    
        If Knro > *Zeros;                                                                           
         Psw = *On;                                                                                 
        Endif;                                                                                      
                                                                                                    
        Return Psw;                                                                                 
      /End-Free                                                                                     
     PPrVtmgldf        E                                                                            
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //Prototipo que verifica que el pago no este varias veces           //                        
      //////////////////////////////////////////////////////////////////////                        
     PPrvtwpgef        B                                                                            
     DPrvtwpgef        Pi              n                                                            
     DPsw              s               n    Inz(*Off)                                               
     DKnro             s              4  0 Inz(*Zeros)                                              
      /Free                                                                                         
        If W_ValTra > 0;                                                                            
         Exec Sql                                                                                   
          Select Count(0) Into :Knro                                                                
          From Votrea00/Vtwpgef                                                                     
          Where Pgecia = :Pcia And Pgecmp = :Pgecmp And                                             
                Pgecdg = :Pgecdg And Pgevlp = :W_ValTra And                                         
                Pgezng = :Pgezng And Pgesec = :Pgesec;                                              
         If Knro > *Zeros;                                                                          
          Psw = *On;                                                                                
         Endif;                                                                                     
        Else;                                                                                       
         Psw = *On;                                                                                 
        Endif;                                                                                      
                                                                                                    
        Return Psw;                                                                                 
      /End-Free                                                                                     
     PPrvtwpgef        E                                                                            
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //Prototipo que verifica las Cuentas de las Lideres                 //                        
      //////////////////////////////////////////////////////////////////////                        
     PPrRevalidarCuentas...                                                                         
     P                 B                                                                            
     DPrRevalidarCuentas...                                                                         
     D                 Pi                                                                           
     DUUsw             s               n   Inz(*Off)                                                
      /Free                                                                                         
       SetLl *Loval Vtwpgef;                                                                        
       Read Vtwpgef;                                                                                
       Dow Not %Eof(Vtwpgef);                                                                       
        If Pgeest = 'P';                                                                            
         UUsw = *Off;                                                                               
         Chain(n) (Pgecia:Pgecdg) Vtmipgf;                                                          
         If %Found(Vtmipgf);                                                                        
          If Pgecdt <> Ipgcdc;                                                                      
           UUsw = *On;                                                                              
           Pgecdt = Ipgcdc;                                                                         
           Chain(n) (Pgecia:Pgecdt) Vtmtpcf;                                                        
           If %Found(Vtmtpcf);                                                                      
            Pgedst = Tpcdes;                                                                        
           Else;                                                                                    
            Pgedst = ' ';                                                                           
           EndIf;                                                                                   
          Endif;                                                                                    
          If Pgecdb <> Ipgcdb;                                                                      
           UUsw = *On;                                                                              
           Pgecdb = Ipgcdb;                                                                         
           Chain(n) (Pgecia:Pgecdb) Vtmbnsf;                                                        
           If %Found(Vtmbnsf);                                                                      
            Pgedsb = Bnsdes;                                                                        
           Else;                                                                                    
            Pgedsb = ' ';                                                                           
           EndIf;                                                                                   
          Endif;                                                                                    
          If Pgenrc <> Ipgnrc;                                                                      
           UUsw = *On;                                                                              
           Pgenrc = Ipgnrc;                                                                         
          Endif;                                                                                    
         Endif;                                                                                     
         If UUsw = *On;                                                                             
          UUsw = *Off;                                                                              
          Update Regpge;                                                                            
         Endif;                                                                                     
        Endif;                                                                                      
        Read Vtwpgef;                                                                               
       Enddo;                                                                                       
      /End-Free                                                                                     
     PPrRevalidarCuentas...                                                                         
     P                 E                                                                            
                                                                                                    
       //-------------------------------------------------------------------//                      
       //Nombre Proc.: Procesar Registros Marcados en Error                 //                      
       //---------------------------------------------------------------------                      
       Dcl-Proc ProcesarCasosMarcadosEnError;                                                       
        Dcl-s NroP Zoned(5:0);                                                                      
        Dcl-s NroP1 Zoned(5:0);                                                                     
        Dcl-s Spgciai Char(3);                                                                      
        Dcl-s Spgzoni Char(3);                                                                      
        Dcl-s Spgseci Char(1);                                                                      
        Dcl-s Spgtrri Char(1);                                                                      
        Dcl-s Spgcedi Zoned(9:0);                                                                   
        Dcl-s Spgcami Char(4);                                                                      
                                                                                                    
       Exec Sql                                                                                     
        Declare CurErr Cursor For                                                                   
        Select Spgcia, Spgzon, Spgsec, Spgtrr, Spgced, Spgcam                                       
        From Votrea00/Vtmspgf                                                                       
        Where Spgcia = :Pcia And Spginp = 'E' And                                                   
              Spgcam >= '1615' And Spgvps > 0                                                       
        Order By Spgcia, Spgced ,Spgcam Desc;                                                       
                                                                                                    
       Exec Sql                                                                                     
        Open CurErr;                                                                                
                                                                                                    
       Exec Sql                                                                                     
        Fetch CurErr Into :Spgciai, :Spgzoni, :Spgseci, :Spgtrri, :Spgcedi,                         
                          :Spgcami;                                                                 
                                                                                                    
       Dow SqlCod <> 100 And SqlCod >= *Zeros;                                                      
        NroP = *Zeros;                                                                              
        NroP1 = *Zeros;                                                                             
        Exec Sql                                                                                    
         Select Count(0) Into :NroP                                                                 
         From Votrea00/Vtwpcgf                                                                      
         Where Pcgcia = :Spgciai And                                                                
               Pcgcmp = :Spgcami And                                                                
               Pcgzna = :Spgzoni And                                                                
               Pcgcdg = :Spgcedi And                                                                
               Pcgind = 'N';                                                                        
                                                                                                    
        Exec Sql                                                                                    
         Select Count(0) Into :NroP1                                                                
         From Votrea00/Vttpprf                                                                      
         Where Pprcia = :Spgciai And                                                                
               Pprcmp = :Spgcami And                                                                
               Pprzna = :Spgzoni And                                                                
               Pprcdg = :Spgcedi;                                                                   
                                                                                                    
        If NroP = 0 And NroP1 = 0;                                                                  
         Exec Sql                                                                                   
         Update Vtmspgf Set Spginp = ' '                                                            
         Where Spgcia = :Spgciai And                                                                
               Spgzon = :Spgzoni And                                                                
               Spgcam = :Spgcami And                                                                
               Spgsec = :Spgseci And                                                                
               Spgced = :Spgcedi And                                                                
               Spgccr = 042;                                                                        
                                                                                                    
         Exec Sql                                                                                   
         Update Vtmspgf Set Spginp = 'T'                                                            
         Where Spgcia = :Spgciai And                                                                
               Spgzon = :Spgzoni And                                                                
               Spgcam = :Spgcami And                                                                
               Spgsec = :Spgseci And                                                                
               Spgced = :Spgcedi And                                                                
               Spgccr = 043;                                                                        
        Endif;                                                                                      
                                                                                                    
        Exec Sql                                                                                    
         Fetch CurErr Into :Spgciai, :Spgzoni, :Spgseci, :Spgtrri, :Spgcedi,                        
                          :Spgcami;                                                                 
       Enddo;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close CurErr;                                                                               
                                                                                                    
       End-Proc ProcesarCasosMarcadosEnError;                                                       
                                                                                                    
       //-------------------------------------------------------------------//                      
       //Nombre Proc.: No Muestra Pagos Pendientes Lideres Activas          //                      
       //---------------------------------------------------------------------                      
       Dcl-Proc PrValidarLiderActiva;                                                               
        Dcl-s Qnro Zoned(5:0);                                                                      
        Dcl-s Rrn Zoned(9:0);                                                                       
        Dcl-s Wcampa Char(4);                                                                       
        Dcl-s WcampaP Char(4);                                                                      
        Dcl-s Spgciah Char(3);                                                                      
        Dcl-s Spgzonh Char(3);                                                                      
        Dcl-s Spgsech Char(1);                                                                      
        Dcl-s Spgcamh Char(4);                                                                      
        Dcl-s Spgcedh Zoned(9:0);                                                                   
                                                                                                    
       Exec Sql                                                                                     
        Declare C1Rr Cursor For                                                                     
        Select Spgcia, Spgzon, Spgsec, Spgcam, Spgced, Rrn(Vtmspgf)                                 
        From Votrea00/Vtmspgf                                                                       
        Where Spgcia = :QSpgcia And                                                                 
              Spgzon = :QSpgzon And                                                                 
              Spgcam = :QSpgcam;                                                                    
                                                                                                    
       Exec Sql                                                                                     
        Open C1Rr;                                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Fetch C1Rr Into :Spgciah, :Spgzonh, :Spgsech, :Spgcamh, :Spgcedh,                           
                        :Rrn;                                                                       
                                                                                                    
       Dow SqlCod <> 100 And SqlCod >= *Zeros;                                                      
        Clear Qnro;                                                                                 
        Clear Wcampa;                                                                               
        Exec Sql                                                                                    
         Select Max(Gldcdt), Count(0) Into :Wcampa, :Qnro                                           
         From Votrea00/Vtmgldf                                                                      
         Where Gldcia = :Spgciah And Gldest = 'D' And                                               
               Gldzon = :Spgzonh And Gldcte = :Spgcedh And                                          
               Gldsct = :Spgsech                                                                    
         Group By Gldcdt;                                                                           
                                                                                                    
        PCamE = Wcampa;                                                                             
        PCamR = *Blanks;                                                                            
        POper = '-';                                                                                
        If Wcampa <= '2012';                                                                        
         Pnume = '4';                                                                               
        Else;                                                                                       
         Pnume = '3';                                                                               
        Endif;                                                                                      
        ProCampa(Pcia:PCamE:PCamR:POper:Pnume);                                                     
                                                                                                    
        Clear WcampaP;                                                                              
        Exec Sql                                                                                    
         Select Spgcam Into :WcampaP                                                                
         From Votrea00/Vtmspgf                                                                      
         Where Spgcia = :Spgciah And                                                                
               Spgzon = :Spgzonh And                                                                
               Spgsec = :Spgsech                                                                    
         Order By Spgcam Desc                                                                       
         Fetch First 1 Row Only;                                                                    
                                                                                                    
        If WcampaP < PCamr And Qnro > 0 And Wcampa <> ' ';                                          
         Exec  Sql                                                                                  
          Update Votrea00/Vtmspgf Set Spginp = 'K'                                                  
          Where Rrn(Vtmspgf) = :Rrn And Spgvps > 0 And Spginp = ' ';                                
        Endif;                                                                                      
        Exec Sql                                                                                    
        Fetch C1Rr Into :Spgciah, :Spgzonh, :Spgsech, :Spgcamh, :Spgcedh,                           
                        :Rrn;                                                                       
       Enddo;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close C1Rr;                                                                                 
                                                                                                    
       End-Proc PrValidarLiderActiva;                                                               
                                                                                                    
       //-------------------------------------------------------------------//                      
       //Nombre Proc.: No Valida si Realmente Cumplio las Politicas Comercia//                      
       //---------------------------------------------------------------------                      
       Dcl-Proc PrVtwpcgf;                                                                          
        Dcl-Pi *N;                                                                                  
         PciaW Char(3);                                                                             
         Zona Char(3);                                                                              
         Campa Char(4);                                                                             
         Ced Zoned(9:0);                                                                            
         Secto Char(1);                                                                             
        End-Pi;                                                                                     
                                                                                                    
        Dcl-s Dngcia Char(3);                                                                       
        Dcl-s Dngcmp Char(4);                                                                       
        Dcl-s Dngcdg Zoned(9:0);                                                                    
        Dcl-s Dngcdr Zoned(9:0);                                                                    
        Dcl-s Knro Zoned(9:0);                                                                      
        Dcl-s Rrn Zoned(9:0);                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Declare C1Rv Cursor For                                                                     
        Select Dngcia, Dngcmp, Dngcdg, Dngcdr, Rrn(Vttdngf)                                         
        From Votrea00/Vttdngf                                                                       
        Where Dngcia = :Pcia And                                                                    
              Dngzng = :Zona And                                                                    
              Dngcmp = :Campa And                                                                   
              Dngcdg = :Ced And                                                                     
              Dngscg = :Secto;                                                                      
                                                                                                    
       Exec Sql                                                                                     
        Open C1Rv;                                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Fetch C1Rv Into :Dngcia, :Dngcmp, :Dngcdg, :Dngcdr, :Rrn;                                   
                                                                                                    
       Dow SqlCod <> 100 And SqlCod >= *Zeros;                                                      
        Clear Knro;                                                                                 
        Exec Sql                                                                                    
         Select Count(0) Into :Knro                                                                 
         From Votrea00/Vtwpcgf                                                                      
         Where Pcgcia = :Dngcia And                                                                 
               Pcgcmp = :Dngcmp And                                                                 
               Pcgcdg = :Dngcdg And                                                                 
               Pcgcdp = :Dngcdr And                                                                 
               Pcgind = 'S';                                                                        
                                                                                                    
        If Knro > 0;                                                                                
         Exec Sql                                                                                   
          Delete From Votrea00/Vttdngf                                                              
          Where Rrn(Vttdngf) = :Rrn;                                                                
        Endif;                                                                                      
                                                                                                    
       Exec Sql                                                                                     
        Fetch C1Rv Into :Dngcia, :Dngcmp, :Dngcdg, :Dngcdr, :Rrn;                                   
       Enddo;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close C1Rv;                                                                                 
                                                                                                    
       End-Proc PrVtwpcgf;                                                                          
                                                                                                    
       //-------------------------------------------------------------------//                      
       //Valida que esten los 2 Movimientos +43 y -043                      //                      
       //---------------------------------------------------------------------                      
       Dcl-Proc PrVtwcabf2;                                                                         
        Dcl-Pi *N;                                                                                  
         PciaW Char(3);                                                                             
        End-Pi;                                                                                     
                                                                                                    
        Dcl-s Wsql Char(2000);                                                                      
        Dcl-s Wcbbco Char(3);                                                                       
        Dcl-s Wcbced Zoned(15:0);                                                                   
        Dcl-s Wcbord Zoned(15:0);                                                                   
        Dcl-s Wcbcedq Zoned(15:0);                                                                  
        Dcl-s Wcbtipq Char(2);                                                                      
        Dcl-s Wcbvalq Zoned(11:2);                                                                  
        Dcl-s Wcbordq Zoned(9:0);                                                                   
        Dcl-s Wcbbcoq Char(3);                                                                      
        Dcl-s Wcbcmnq Char(20);                                                                     
        Dcl-s Wcbfetq Char(10);                                                                     
        Dcl-s Wcbstsq Char(1);                                                                      
        Dcl-s Wcbusrq Char(10);                                                                     
        Dcl-s Wcbcctq Zoned(3:0);                                                                   
                                                                                                    
        Clear Wsql;                                                                                 
        Wsql = 'Select Wcbbco, Wcbced, Wcbord ' +                                                   
               'From Votrea' + PciaW + '/Vtwcabf2 ' +                                               
               'Where Wcbcct = ' + '043' + ' ' +                                                    
               'Group By Wcbbco, Wcbced, Wcbord, Wcbtip ' +                                         
               'Having Count(Wcbced) = 1';                                                          
                                                                                                    
        Exec Sql                                                                                    
         Prepare CMc From :Wsql;                                                                    
                                                                                                    
        Exec Sql                                                                                    
         Declare Cmc1 Scroll Cursor For CMc;                                                        
                                                                                                    
        Exec Sql                                                                                    
         Open Cmc1;                                                                                 
                                                                                                    
        Exec Sql                                                                                    
         Fetch Cmc1 Into :Wcbbco, :Wcbced, :Wcbord;                                                 
                                                                                                    
        Dow Sqlcod <> 100 And Sqlcod >= *Zeros;                                                     
         Clear Wsql;                                                                                
         Wsql = 'Select Wcbced, Wcbtip, Wcbval, ' +                                                 
                'Wcbord, Wcbbco, Wcbcmn, Wcbfet, ' +                                                
                'Wcbsts, Wcbusr, Wcbcct ' +                                                         
                'From Votrea' + PciaW + '/Vtwcabf2 ' +                                              
                'Where Wcbbco = ' + '''' + %Trim(Wcbbco) + '''' + ' And ' +                         
                'Wcbced = ' + %Trim(%Editc(Wcbced:'Z')) + ' And ' +                                 
                'Wcbcct = ' + '043';                                                                
                                                                                                    
         Exec Sql                                                                                   
          Prepare CMc1 From :Wsql;                                                                  
                                                                                                    
         Exec Sql                                                                                   
          Declare Cmc11 Scroll Cursor For CMc1;                                                     
                                                                                                    
         Exec Sql                                                                                   
          Open Cmc11;                                                                               
                                                                                                    
         Exec Sql                                                                                   
          Fetch Cmc11 Into :Wcbcedq, :Wcbtipq, :Wcbvalq, :Wcbordq,                                  
                           :Wcbbcoq, :Wcbcmnq, :Wcbfetq, :Wcbstsq,                                  
                           :Wcbusrq, :Wcbcctq;                                                      
                                                                                                    
         If SqlCod <> 100 And SqlCod > *Zeros;                                                      
          If Wcbtipq = 'A+';                                                                        
           Wcbtipq = 'A-';                                                                          
           Wcbvalq = Wcbvalq * (-1);                                                                
          Endif;                                                                                    
          If Wcbtipq = 'A-';                                                                        
           Wcbtipq = 'A+';                                                                          
           Wcbvalq = Wcbvalq * (-1);                                                                
          Endif;                                                                                    
          Chain (Wcbbcoq:Wcbcedq:Wcbordq:Wcbtipq) Vtwcabf2;                                         
          If Not %Found(Vtwcabf2);                                                                  
           Wcbced = Wcbcedq;                                                                        
           Wcbtip = Wcbtipq;                                                                        
           Wcbval = Wcbvalq;                                                                        
           Wcbord = Wcbordq;                                                                        
           Wcbbco = Wcbbcoq;                                                                        
           Wcbcmn = Wcbcmnq;                                                                        
           Wcbfet = Wcbfetq;                                                                        
           Wcbsts = Wcbstsq;                                                                        
           Wcbusr = Wcbusrq;                                                                        
           Wcbcct = Wcbcctq;                                                                        
           Write Rwcab2;                                                                            
          Endif;                                                                                    
         Endif;                                                                                     
                                                                                                    
         Exec Sql                                                                                   
          Close Cmc11;                                                                              
                                                                                                    
         Exec Sql                                                                                   
          Fetch Cmc1 Into :Wcbbco, :Wcbced, :Wcbord;                                                
        Enddo;                                                                                      
                                                                                                    
        Exec Sql                                                                                    
         Close Cmc1;                                                                                
                                                                                                    
       End-Proc PrVtwcabf2;                                                                         
