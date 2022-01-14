***************************************************************************                         
     **                                                                   *                         
     ** Programa      : VTMCCC2R                                          *                         
     ** Descripciòn:  : Actualizar Cartera                                *                         
     ** Proyecto      : Menú de Opciones Opi Paises                       *                         
     ** Autor         : (Jortega)                                         *                         
     ** Fecha Creaciòn: 29 de Diciembre de 2008                           *                         
     **                                                                   *                         
     **********************************************************************                         
     ** Definición de Directivas de Compilación                                                     
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
                                                                                                    
     **Definiciòn de Archivos de Trabajo                                                            
     FLmllst01  If   e           k Disk                                                             
     FLgladj01  If   e           k Disk                                                             
     FVtmtpnf   If   e           k Disk                                                             
     FVtmbncf   If   e           k Disk                                                             
     FVlardtl01 If   e           k Disk    Rename(Pardtlf1:Regpardtl)                               
     FVtwnvcf   Uf a e             Disk                                                             
     FVtwcabf2  If a e           k Disk                                                             
                                                                                                    
     ** Definiciòn de Archivo de Pantallas                                                          
     FVtacci0p  Cf   e             Workstn                                                          
     F                                     Sfile(Rdatos:Nrr)                                        
     F                                     Sfile(Rdatos1:Nrr1)                                      
     F                                     Sfile(Rdatos2:Nrr1)                                      
     F                                     Sfile(Rdatos3:Nrr1)                                      
     F                                     Sfile(Rdatos4:Nrr1)                                      
                                                                                                    
     ** Definiciòn de Variables de Trabajo                                                          
     DPced1            s              9s 0 Inz(*Zeros)                                              
     DXsw              s              1    Inz(*Blanks)                                             
     DPsw              s              1    Inz(*Blanks)                                             
     DXcia             s              3    Inz(*Blanks)                                             
     DXpais            s             20    Inz(*Blanks)                                             
     DSqlStr           s           1000a   Inz(*Blanks)                                             
     DSqlNr1           s            500a   Inz(*Blanks)                                             
     DSqlNr2           s            500a   Inz(*Blanks)                                             
     DPtr              s               *   Inz(%Addr(*In))                                          
     DTced             s             30    Inz(*Blanks)                                             
     DTcns             s              9    Inz(*Blanks)                                             
     DWres             s              2a   Inz(*Blanks)                                             
     DWvalor           s            100a   Inz(*Blanks)                                             
                                                                                                    
     ** Definiciòn de Estructuras de Trabajo                                                        
     D                Sds                                                                           
     DJob                    244    253                                                             
     DUser                   254    263                                                             
     DSig_sys                195    196  0                                                          
     DMes_sys                276    277  0                                                          
     DDia_sys                278    279  0                                                          
     DAÑo_sys                280    281  0                                                          
                                                                                                    
     DIndic            Ds                  Based(Ptr)                                               
     DSalir                            N   Overlay(Indic:3)                                         
     DCrear                            N   Overlay(Indic:06)                                        
     DCancelar                         N   Overlay(Indic:12)                                        
     DMsg1                             N   Overlay(Indic:38)                                        
     DMsg2                             N   Overlay(Indic:39)                                        
     DMsg4                             N   Overlay(Indic:54)                                        
     DMsg5                             N   Overlay(Indic:52)                                        
     DSflDsp                           N   Overlay(Indic:85)                                        
     DSflDspCtl                        N   Overlay(Indic:80)                                        
     DSflClr                           N   Overlay(Indic:75)                                        
     DSflEnd                           N   Overlay(Indic:40)                                        
                                                                                                    
     ** Definiciòn de Estructuras de Trabajo                                                        
     DFields           Ds                                                                           
     DQmlzipc                              Like(Mlzipc)                                             
     DQmlcstÑ                              Like(MlcstÑ)                                             
     DQmlname                              Like(Mlname)                                             
                                                                                                    
     DFieldsFile       Ds                                                                           
     DQNvccia                              Like(Nvccia)                                             
     DQNvcord                              Like(Nvcord)                                             
     DQNvctpn                              Like(Nvctpn)                                             
     DQNvcvlr                              Like(Nvcvlr)                                             
     DQNvccmt                              Like(Nvccmt)                                             
     DQNvccbc                              Like(Nvccbc)                                             
     DQNvcnbc                              Like(Nvcnbc)                                             
     DQNvcccb                              Like(Nvcccb)                                             
     DQNvcncb                              Like(Nvcncb)                                             
     DQNvcced                              Like(Nvcced)                                             
                                                                                                    
      ** Fecha Transacción                                                                          
     D                 Ds                                                                           
     D Adtryy                  1      2  0                                                          
     D Wsepa1                  3      3    Inz('/')                                                 
     D Adtrmm                  4      5  0                                                          
     D Wsepa2                  6      6    Inz('/')                                                 
     D Adtrdd                  7      8  0                                                          
     D Wfetr                   1      8                                                             
                                                                                                    
      ** Fecha Vencimiento                                                                          
     D                 Ds                                                                           
     D Adduyy                  1      2  0                                                          
     D Wsepa3                  3      3    Inz('/')                                                 
     D Addumm                  4      5  0                                                          
     D Wsepa4                  6      6    Inz('/')                                                 
     D Addudd                  7      8  0                                                          
     D Wfeve                   1      8                                                             
                                                                                                    
     DMyDtaara         Ds                  Dtaara(Vtdpnc)                                           
     DProceso                  1      1a                                                            
                                                                                                    
     ** Definiciòn de Prototipos de Trabajo                                                         
     DPrDtaara         Pr              n                                                            
     DLlenarSfl        Pr                                                                           
     DBorraSfl         Pr                                                                           
     DPintarSfl        Pr                                                                           
     DEncabezado       Pr                                                                           
     DCrearNov         Pr                                                                           
     DLlenarSflBn      Pr                                                                           
     DBorraSflBn       Pr                                                                           
     DPintarSflBn      Pr                                                                           
     DLlenarSflCc      Pr                                                                           
     DBorraSflCc       Pr                                                                           
     DPintarSflCc      Pr                                                                           
     DGrabarSfl        Pr                                                                           
     DBorrarRegSfl     Pr                                                                           
     DLlenarSflTp      Pr                                                                           
     DBorraSflTp       Pr                                                                           
     DPintarSflTp      Pr                                                                           
     DLlenarSflOr      Pr                                                                           
     DBorraSflOr       Pr                                                                           
     DPintarSflOr      Pr                                                                           
     DAfectarCartera   Pr                                                                           
                                                                                                    
     DQcmdexc          Pr                  Extpgm('QCMDEXC')                                        
     D                             3000a   Const Options(*Varsize)                                  
     D                               15p 5 Const                                                    
     D                                3a   Const Options(*Nopass)                                   
                                                                                                    
     DPgmPais          Pr                  Extpgm('VOTREP00/VTMCSBBR')                              
     DXcia                            3                                                             
     DXpais                          20                                                             
                                                                                                    
     DVerifica         Pr                  Extpgm('VOTREP00/VTMGOB9R')                              
     DWres                            2a                                                            
     DWvalor                        100a                                                            
                                                                                                    
     DPgmCed           Pr                  Extpgm('VOTREP00/VTMCSBCR')                              
     DPcia                            3                                                             
     DTced                           30                                                             
     DTcns                            9                                                             
                                                                                                    
     DMain             Pr                  Extpgm('Main')                                           
     DPcia                            3a                                                            
     DPacc                            1a                                                            
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
     DPcia                            3a                                                            
     DPacc                            1a                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       //              P r o g r a m a   P r i n c i p a l                 //                       
       //------------------------------------------------------------------//                       
      /Free                                                                                         
       If PrDtaara() = *On;                                                                         
         PgmPais(Pcia:Xpais);                                                                       
         Dow Salir = *Off;                                                                          
          Pfecha = %Char(%Date());                                                                  
          If Pacc = 'P';                                                                            
           Ptit = '(Pagos)';                                                                        
          Else;                                                                                     
           Ptit = '(Ajustes)';                                                                      
          Endif;                                                                                    
          PintarSfl();                                                                              
          If Pced = *Blanks And Salir = *Off;                                                       
           Msg2 = *On;                                                                              
           Exfmt Rdato1;                                                                            
           Msg2 = *Off;                                                                             
           Clear Rcontrol;                                                                          
           Iter;                                                                                    
          Endif;                                                                                    
          If Nrr <= *Zeros And Salir = *Off And Xsw = *Blanks And                                   
          Pced <> *Blanks;                                                                          
           Msg1 = *On;                                                                              
           Exfmt Rdato1;                                                                            
           Msg1 = *Off;                                                                             
           Clear Rcontrol;                                                                          
           Iter;                                                                                    
          Endif;                                                                                    
          If Salir = *On;                                                                           
           Leave;                                                                                   
          Endif;                                                                                    
          If Cancelar = *On;                                                                        
           Pced1 = *Zeros;                                                                          
           Tcns = *Blanks;                                                                          
           Clear Rcontrol;                                                                          
           Iter;                                                                                    
          Endif;                                                                                    
         Enddo;                                                                                     
        Endif;                                                                                      
        *Inlr = *On;                                                                                
      /End-Free                                                                                     
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Llena el Sub Archivo del Sub File              //                       
       //------------------------------------------------------------------//                       
     PLlenarSfl        B                                                                            
     DLlenarSfl        Pi                                                                           
      /Free                                                                                         
       Encabezado();                                                                                
       If Psw = *Blanks;                                                                            
        BorraSfl();                                                                                 
        Exec Sql                                                                                    
        Declare C1 Scroll Cursor For                                                                
        Select Nvccia, Nvcord, Nvctpn, Nvcvlr, Nvccmt, Nvccbc, Nvcnbc,                              
               Nvcccb, Nvcncb, Nvcced                                                               
        From Votrea00/Vtwnvcf                                                                       
        Where Nvccia = :Pcia And Nvcced = :Pced1;                                                   
                                                                                                    
        Exec Sql                                                                                    
         Open C1;                                                                                   
                                                                                                    
        Exec Sql                                                                                    
         Fetch C1 Into :FieldsFile;                                                                 
                                                                                                    
       Dow Sqlcod <> 100;                                                                           
        Pop = *Blanks;                                                                              
        Pord = Qnvcord;                                                                             
        Ptpn = Qnvctpn;                                                                             
        Pvlr = Qnvcvlr;                                                                             
        Pcom = Qnvccmt;                                                                             
        Pban = Qnvcnbc;                                                                             
        Pcbn = Qnvccbc;                                                                             
        Pccble = Qnvcccb;                                                                           
        Nrr += 1;                                                                                   
        Write Rdatos;                                                                               
        Exec Sql                                                                                    
         Fetch C1 Into :FieldsFile;                                                                 
       Enddo;                                                                                       
       SflEnd = *On;                                                                                
                                                                                                    
       Exec Sql                                                                                     
        Close C1;                                                                                   
                                                                                                    
        If Nrr > *Zeros;                                                                            
         SflDsp = *On;                                                                              
        Else;                                                                                       
         SflDsp = *Off;                                                                             
        Endif;                                                                                      
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
       //Pced1 = *Zeros;                                                                            
       Tcns = *Blanks;                                                                              
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
       Ppais = Xpais;                                                                               
       SflDspCtl = *On;                                                                             
       Write Rblanco;                                                                               
       Write Rteclas;                                                                               
       Write Rteclas;                                                                               
       Exfmt Rcontrol;                                                                              
       Msg4 = *Off;                                                                                 
       SflDspCtl = *Off;                                                                            
       If Crear = *On And Xsw <> *Blanks And Psw = *Blanks;                                         
        CrearNov();                                                                                 
       Endif;                                                                                       
       If *In09 = *On And Xsw <> *Blanks And Psw = *Blanks;                                         
        AfectarCartera();                                                                           
       Endif;                                                                                       
       If Salir = *Off And Cancelar = *Off And Nrr > *Zeros;                                        
        Readc Rdatos;                                                                               
         Dow Not %Eof();                                                                            
          Select;                                                                                   
           When Pop = '4';                                                                          
            BorrarRegSfl();                                                                         
          Endsl;                                                                                    
          Pop = *Blanks;                                                                            
          Update Rdatos;                                                                            
         Readc Rdatos;                                                                              
        Enddo;                                                                                      
        Endif;                                                                                      
       If Pced <> *Blanks And Salir = *Off And Cancelar = *Off;                                     
        LlenarSfl();                                                                                
       Endif;                                                                                       
       If Psw <> *Blanks And Salir = *Off And Cancelar = *Off;                                      
        *In57 = *On;                                                                                
        Exfmt Rdato1;                                                                               
        *In57 = *Off;                                                                               
       Endif;                                                                                       
      /End-Free                                                                                     
     PPintarSfl        E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Consulta la Información de la Compradora       //                       
       //------------------------------------------------------------------//                       
     PEncabezado       B                                                                            
     DEncabezado       Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DPcedx            s             30s 0                                                          
      /Free                                                                                         
        Psw = *Blanks;                                                                              
        Xsw = *Blanks;                                                                              
        Pnom = *Blanks;                                                                             
        Pzona = *Blanks;                                                                            
        Pcus = *Blanks;                                                                             
        Pucam = *Blanks;                                                                            
        Pced = %Trim(Pced);                                                                         
        PgmCed(Pcia:Pced:Tcns);                                                                     
        If %Len(%Trim(Pced)) <= *Zeros;                                                             
         Pced = *All'0';                                                                            
        Endif;                                                                                      
        Wres = *Blanks;                                                                             
        WValor = *Blanks;                                                                           
        WValor = Pced;                                                                              
        Verifica(Wres:Wvalor);                                                                      
        If Wres = 'Si';                                                                             
         Pcedx = %Int(%Trim(Pced));                                                                 
        Else;                                                                                       
         Monitor;                                                                                   
          Pcedx = %Int(Tcns);                                                                       
         On-Error;                                                                                  
          Pcedx = *Zeros;                                                                           
         Endmon;                                                                                    
        Endif;                                                                                      
        Pced = %Trim(%Editc(Pcedx:'Z'));                                                            
        Evalr Tcns = %Trim(Tcns);                                                                   
        Monitor;                                                                                    
         Pced1 = %Int(Tcns);                                                                        
        On-Error;                                                                                   
         Pced1 = *Zeros;                                                                            
        Endmon;                                                                                     
        Chain Pced1 Lmllst01;                                                                       
        If %Found(Lmllst01);                                                                        
         Xsw = 'S';                                                                                 
         Msg4 = *On;                                                                                
         Pnom = %Trim(Mlname);                                                                      
         Pzona = %Trim(Mlzipc);                                                                     
         Pcus = %Trim(Mlprlv);                                                                      
         Pucam = %Trim(Mllmed);                                                                     
         If Mldele <> *Blanks;                                                                      
          Psw = 'S';                                                                                
         Endif;                                                                                     
        Endif;                                                                                      
      /End-Free                                                                                     
     PEncabezado       E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Crea Registros para Alterar la Cartera         //                       
       //------------------------------------------------------------------//                       
     PCrearNov         B                                                                            
     DCrearNov         Pi                                                                           
      /Free                                                                                         
       Clear Rdato2;                                                                                
       Dow Salir = *Off;                                                                            
        *In96 = *On;                                                                                
        //Write Rblanco;                                                                            
        Exfmt Rdato2;                                                                               
        *In96 = *Off;                                                                               
        Dow Ptpn1 = *Zeros And Salir = *Off And Cancelar = *Off;                                    
         *In21 = *On;                                                                               
         Pdtpn = 'Seleccione el Tipo de Novedad';                                                   
         Exfmt Rdato2;                                                                              
         *In21 = *Off;                                                                              
        Enddo;                                                                                      
        If Ptpn1 <> *Zeros And *In59 = *On And Salir = *Off And                                     
         Cancelar = *Off;                                                                           
         PintarSflTp();                                                                             
        Endif;                                                                                      
        If Pabre1 <> 'P';                                                                           
        Dow Pcct = *Zeros And Salir = *Off And Cancelar = *Off;                                     
         *In22 = *On;                                                                               
         Pdcct = 'Seleccione el Concepto';                                                          
         Exfmt Rdato2;                                                                              
         *In22 = *Off;                                                                              
        Enddo;                                                                                      
        If Pcct <> *Zeros And *In55 = *On And Salir = *Off And                                      
         Cancelar = *Off;                                                                           
         PintarSflCc();                                                                             
        Endif;                                                                                      
        Endif;                                                                                      
        If Pabre1 = 'P';                                                                            
        Dow Pbnc = *Zeros And Salir = *Off And Cancelar = *Off;                                     
         *In23 = *On;                                                                               
         Pdbnc = 'Seleccione el Banco';                                                             
         Exfmt Rdato2;                                                                              
         *In23 = *Off;                                                                              
        Enddo;                                                                                      
        If Pbnc <> *Zeros And *In56 = *On And Salir = *Off And                                      
         Cancelar = *Off;                                                                           
         PintarSflBn();                                                                             
        Endif;                                                                                      
        Endif;                                                                                      
        Dow Pvlrw = *Zeros And Salir = *Off And Cancelar = *Off;                                    
         *In25 = *On;                                                                               
         Exfmt Rdato2;                                                                              
         *In25 = *Off;                                                                              
        Enddo;                                                                                      
        Dow Pcomw = *Blanks And Salir = *Off And Cancelar = *Off;                                   
         *In26 = *On;                                                                               
         Exfmt Rdato2;                                                                              
         *In26 = *Off;                                                                              
        Enddo;                                                                                      
        If Pcord <> *Zeros And Salir = *Off And Cancelar = *Off;                                    
         PintarSflOr();                                                                             
         Pcord = *Zeros;                                                                            
        Endif;                                                                                      
        If Salir = *On;                                                                             
         Leave;                                                                                     
        Endif;                                                                                      
        If *In08 = *On;                                                                             
         GrabarSfl();                                                                               
        Endif;                                                                                      
        If Cancelar = *On;                                                                          
         Clear Rdato2;                                                                              
         Iter;                                                                                      
        Endif;                                                                                      
       Enddo;                                                                                       
       If Salir = *On;                                                                              
        Salir = *Off;                                                                               
       Endif;                                                                                       
      /End-Free                                                                                     
     PCrearNov         E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Llena el Sub Archivo del Sub File              //                       
       //------------------------------------------------------------------//                       
     PLlenarSflBn      B                                                                            
     DLlenarSflBn      Pi                                                                           
      /Free                                                                                         
       BorraSflBn();                                                                                
       Setll *Loval Vtmbncf;                                                                        
        Read Vtmbncf;                                                                               
        Dow Not %Eof(Vtmbncf);                                                                      
         Pop = *Blanks;                                                                             
         Pcbc = Bnccod;                                                                             
         Pdbc = Bncdes;                                                                             
         Nrr1 += 1;                                                                                 
         Write Rdatos1;                                                                             
        Read Vtmbncf;                                                                               
        Enddo;                                                                                      
        *In50 = *On;                                                                                
        If Nrr1 > *Zeros;                                                                           
         *In95 = *On;                                                                               
        Else;                                                                                       
         *In95 = *Off;                                                                              
        Endif;                                                                                      
      /End-Free                                                                                     
     PLlenarSflBn      E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Borra el Sub Archivo de la Pantalla            //                       
       //------------------------------------------------------------------//                       
     PBorraSflBn       B                                                                            
     DBorraSflBn       Pi                                                                           
      /Free                                                                                         
       Nrr1 = *Zeros;                                                                               
       *In65 = *On;                                                                                 
       Write Rcontrol1;                                                                             
       *In65 = *Off;                                                                                
      /End-Free                                                                                     
     PBorraSflBn       E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Pinta el Sub Archivo de la Pantalla            //                       
       //------------------------------------------------------------------//                       
     PPintarSflBn      B                                                                            
     DPintarSflBn      Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DFsw              s              1a   Inz(*Blanks)                                             
      /Free                                                                                         
       Dow Fsw = *Blanks And Salir = *Off And Cancelar = *Off;                                      
       LlenarSflBn();                                                                               
       *In90 = *On;                                                                                 
       Exfmt Rcontrol1;                                                                             
       Msg4 = *Off;                                                                                 
       *In90 = *Off;                                                                                
       If Salir = *Off And Cancelar = *Off And Nrr1 > *Zeros;                                       
        Readc Rdatos1;                                                                              
         Dow Not %Eof();                                                                            
          Select;                                                                                   
           When Pop = '1';                                                                          
            Fsw = 'S';                                                                              
            Pcbnc = Pcbc;                                                                           
            Pdbnc = Pdbc;                                                                           
          Endsl;                                                                                    
          Pop = *Blanks;                                                                            
          Update Rdatos1;                                                                           
         Readc Rdatos1;                                                                             
        Enddo;                                                                                      
        Endif;                                                                                      
       Enddo;                                                                                       
      /End-Free                                                                                     
     PPintarSflBn      E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Llena el Sub Archivo del Sub File              //                       
       //------------------------------------------------------------------//                       
     PLlenarSflCc      B                                                                            
     DLlenarSflCc      Pi                                                                           
      /Free                                                                                         
       BorraSflCc();                                                                                
       Setll *Loval Lgladj01;                                                                       
        Read Lgladj01;                                                                              
        Dow Not %Eof(Lgladj01);                                                                     
         Pop = *Blanks;                                                                             
         Pccon = Gjadpt;                                                                            
         Pdcc = Gjcmt1;                                                                             
         Nrr1 += 1;                                                                                 
         Write Rdatos2;                                                                             
        Read Lgladj01;                                                                              
        Enddo;                                                                                      
        *In50 = *On;                                                                                
        If Nrr1 > *Zeros;                                                                           
         *In95 = *On;                                                                               
        Else;                                                                                       
         *In95 = *Off;                                                                              
        Endif;                                                                                      
      /End-Free                                                                                     
     PLlenarSflCc      E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Borra el Sub Archivo de la Pantalla            //                       
       //------------------------------------------------------------------//                       
     PBorraSflCc       B                                                                            
     DBorraSflCc       Pi                                                                           
      /Free                                                                                         
       Nrr1 = *Zeros;                                                                               
       *In65 = *On;                                                                                 
       Write Rcontrol2;                                                                             
       *In65 = *Off;                                                                                
      /End-Free                                                                                     
     PBorraSflCc       E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Pinta el Sub Archivo de la Pantalla            //                       
       //------------------------------------------------------------------//                       
     PPintarSflCc      B                                                                            
     DPintarSflCc      Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DFsw              s              1a   Inz(*Blanks)                                             
      /Free                                                                                         
       Dow Fsw = *Blanks;                                                                           
       LlenarSflCc();                                                                               
       *In90 = *On;                                                                                 
       Exfmt Rcontrol2;                                                                             
       Msg4 = *Off;                                                                                 
       *In90 = *Off;                                                                                
       If Salir = *Off And Cancelar = *Off And Nrr1 > *Zeros;                                       
        Readc Rdatos2;                                                                              
         Dow Not %Eof();                                                                            
          Select;                                                                                   
           When Pop = '1';                                                                          
            Fsw = 'S';                                                                              
            Pccct = Pccon;                                                                          
            Pdcct = Pdcc;                                                                           
          Endsl;                                                                                    
          Pop = *Blanks;                                                                            
          Update Rdatos2;                                                                           
         Readc Rdatos2;                                                                             
        Enddo;                                                                                      
        Endif;                                                                                      
       Enddo;                                                                                       
      /End-Free                                                                                     
     PPintarSflCc      E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Graba Información en el Sfl                    //                       
       //------------------------------------------------------------------//                       
     PGrabarSfl        B                                                                            
     DGrabarSfl        Pi                                                                           
      /Free                                                                                         
       If Pordw = *Zeros And Pabre1 <> 'P';                                                         
        Pordw = 888888888;                                                                          
       Endif;                                                                                       
       If Pabre1 = 'P' Or Pabre1 = 'A-';                                                            
        Pvlrw = (Pvlrw * (-1));                                                                     
       Endif;                                                                                       
       Exec Sql                                                                                     
       Insert Into Vtwnvcf(                                                                         
       Nvccia,                                                                                      
       Nvcord,                                                                                      
       Nvctpn,                                                                                      
       Nvcvlr,                                                                                      
       Nvccmt,                                                                                      
       Nvccbc,                                                                                      
       Nvcnbc,                                                                                      
       Nvcccb,                                                                                      
       Nvcncb,                                                                                      
       Nvcced)                                                                                      
       Values(:Pcia,                                                                                
              :Pordw,                                                                               
              :Pabre1,                                                                              
              :Pvlrw,                                                                               
              :Pcomw,                                                                               
              :Pcbnc,                                                                               
              :Pdbnc,                                                                               
              :Pccct,                                                                               
              :Pdcct,                                                                               
              :Pced1);                                                                              
       *In27 = *On;                                                                                 
       Exfmt Rdato2;                                                                                
       *In27 = *Off;                                                                                
       Clear Rdato2;                                                                                
      /End-Free                                                                                     
     PGrabarSfl        E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Borra Registro del Sub File                    //                       
       //------------------------------------------------------------------//                       
     PBorrarRegSfl     B                                                                            
     DBorrarRegSfl     Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
      /Free                                                                                         
       Clear Rdato3;                                                                                
       Dow Popc = *Blanks;                                                                          
        Exfmt Rdato3;                                                                               
        If Popc = 's' Or Popc = 'S';                                                                
         Exec Sql                                                                                   
          Delete From Votrea00/Vtwnvcf Where Nvccia = :Pcia And                                     
          Nvcced = :Pced1 And Nvctpn = :Ptpn And Nvcccb = :Pccble And                               
          Nvcvlr = :Pvlr;                                                                           
          *In39 = *On;                                                                              
          Exfmt Rdato3;                                                                             
          *In39 = *Off;                                                                             
        Endif;                                                                                      
       Enddo;                                                                                       
      /End-Free                                                                                     
     PBorrarRegSfl     E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Llena el Sub Archivo del Sub File              //                       
       //------------------------------------------------------------------//                       
     PLlenarSflTp      B                                                                            
     DLlenarSflTp      Pi                                                                           
      /Free                                                                                         
       BorraSflTp();                                                                                
       Setll *Loval Vtmtpnf;                                                                        
        Read Vtmtpnf;                                                                               
        Dow Not %Eof(Vtmtpnf);                                                                      
         If Tpnest = *Blanks And Tpncia = Pcia;                                                     
          If Pacc = %Subst(Tpnabr:1:1);                                                             
           Pop = *Blanks;                                                                           
           Pctpn = Tpncod;                                                                          
           Pdtpn1 = Tpndes;                                                                         
           Pabre = Tpnabr;                                                                          
           Nrr1 += 1;                                                                               
           Write Rdatos3;                                                                           
          Endif;                                                                                    
         Endif;                                                                                     
        Read Vtmtpnf;                                                                               
        Enddo;                                                                                      
        *In50 = *On;                                                                                
        If Nrr1 > *Zeros;                                                                           
         *In95 = *On;                                                                               
        Else;                                                                                       
         *In95 = *Off;                                                                              
        Endif;                                                                                      
      /End-Free                                                                                     
     PLlenarSflTp      E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Borra el Sub Archivo de la Pantalla            //                       
       //------------------------------------------------------------------//                       
     PBorraSflTp       B                                                                            
     DBorraSflTp       Pi                                                                           
      /Free                                                                                         
       Nrr1 = *Zeros;                                                                               
       *In65 = *On;                                                                                 
       Write Rcontrol3;                                                                             
       *In65 = *Off;                                                                                
      /End-Free                                                                                     
     PBorraSflTp       E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Pinta el Sub Archivo de la Pantalla            //                       
       //------------------------------------------------------------------//                       
     PPintarSflTp      B                                                                            
     DPintarSflTp      Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DFsw              s              1a   Inz(*Blanks)                                             
      /Free                                                                                         
       Dow Fsw = *Blanks;                                                                           
       LlenarSflTp();                                                                               
       *In90 = *On;                                                                                 
       Exfmt Rcontrol3;                                                                             
       Msg4 = *Off;                                                                                 
       *In90 = *Off;                                                                                
       If Salir = *Off And Cancelar = *Off And Nrr1 > *Zeros;                                       
        Readc Rdatos3;                                                                              
         Dow Not %Eof();                                                                            
          Select;                                                                                   
           When Pop = '1';                                                                          
            Fsw = 'S';                                                                              
            Pctpn1 = Pctpn;                                                                         
            Pdtpn = Pdtpn1;                                                                         
            Pabre1 = Pabre;                                                                         
          Endsl;                                                                                    
          Pop = *Blanks;                                                                            
          Update Rdatos3;                                                                           
         Readc Rdatos3;                                                                             
        Enddo;                                                                                      
        Endif;                                                                                      
        If Salir = *On;                                                                             
         Salir = *Off;                                                                              
         Fsw = 'S';                                                                                 
       Endif;                                                                                       
       Enddo;                                                                                       
      /End-Free                                                                                     
     PPintarSflTp      E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Llena el Sub Archivo del Sub File              //                       
       //------------------------------------------------------------------//                       
     PLlenarSflOr      B                                                                            
     DLlenarSflOr      Pi                                                                           
                                                                                                    
      /Free                                                                                         
       BorraSflOr();                                                                                
       Setll Pced1 Vlardtl01;                                                                       
        Reade Pced1 Vlardtl01;                                                                      
        Dow Not %Eof(Vlardtl01);                                                                    
         If Nrr1 < 9999;                                                                            
          Pop = *Blanks;                                                                            
          Pxtip = Adtrty;                                                                           
          Pzord = AdordÑ;                                                                           
          Pxval = Adtram;                                                                           
          Pxcpt = Adrdpt;                                                                           
          Pxbnc = Adtend;                                                                           
          If Adtrmm > *Zeros Or Adtrdd > *Zeros;                                                    
           Pxftra = Wfetr;                                                                          
          Endif;                                                                                    
          If Addumm > *Zeros Or Addudd > *Zeros;                                                    
           Pxftra = Wfeve;                                                                          
          Endif;                                                                                    
          Pxcam = %Subst(Addept:1:4);                                                               
          Pxfac = %Subst(Addept:5:6);                                                               
          Nrr1 += 1;                                                                                
          Write Rdatos4;                                                                            
         Endif;                                                                                     
        Reade Pced1 Vlardtl01;                                                                      
        Enddo;                                                                                      
        *In50 = *On;                                                                                
        If Nrr1 > *Zeros;                                                                           
         *In95 = *On;                                                                               
        Else;                                                                                       
         *In95 = *Off;                                                                              
        Endif;                                                                                      
      /End-Free                                                                                     
     PLlenarSflOr      E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Borra el Sub Archivo de la Pantalla            //                       
       //------------------------------------------------------------------//                       
     PBorraSflOr       B                                                                            
     DBorraSflOr       Pi                                                                           
      /Free                                                                                         
       Nrr1 = *Zeros;                                                                               
       *In65 = *On;                                                                                 
       Write Rcontrol4;                                                                             
       *In65 = *Off;                                                                                
      /End-Free                                                                                     
     PBorraSflOr       E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Pinta el Sub Archivo de la Pantalla            //                       
       //------------------------------------------------------------------//                       
     PPintarSflOr      B                                                                            
     DPintarSflOr      Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DFsw              s              1a   Inz(*Blanks)                                             
      /Free                                                                                         
       Dow Salir = *Off;                                                                            
       LlenarSflOr();                                                                               
       Pcced = Pced;                                                                                
       Pcnom = Pnom;                                                                                
       *In90 = *On;                                                                                 
       Write Rblanco;                                                                               
       Write Rteclas1;                                                                              
       Exfmt Rcontrol4;                                                                             
       Msg4 = *Off;                                                                                 
       *In90 = *Off;                                                                                
       If Salir = *Off And Cancelar = *Off And Nrr1 > *Zeros;                                       
        Readc Rdatos4;                                                                              
         Dow Not %Eof();                                                                            
          Select;                                                                                   
           When Pop = '1';                                                                          
            Fsw = 'S';                                                                              
            Pordw = Pzord;                                                                          
            Salir = *On;                                                                            
          Endsl;                                                                                    
          Pop = *Blanks;                                                                            
          Update Rdatos4;                                                                           
         Readc Rdatos4;                                                                             
        Enddo;                                                                                      
        Endif;                                                                                      
       Enddo;                                                                                       
       If Salir = *On;                                                                              
        Salir = *Off;                                                                               
       Endif;                                                                                       
      /End-Free                                                                                     
     PPintarSflOr      E                                                                            
                                                                                                    
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
     DKord             s              9s 0 Inz(*Zeros)                                              
     DKtip             s              2a   Inz(*Blanks)                                             
     DCmd              s           3000a                                                            
     DLen              s             15p 5                                                          
     DWvari            s            200a   Inz(*Blanks)                                             
                                                                                                    
     ** Definiciòn de Prototipos de Trabajo                                                         
     DQcmdexc          Pr                  Extpgm('QCMDEXC')                                        
     D                             3000a   Const Options(*Varsize)                                  
     D                               15p 5 Const                                                    
     D                                3a   Const Options(*Nopass)                                   
      /Free                                                                                         
       Evalr Wsig = %Char(Sig_sys);                                                                 
       Wsig = %Xlate(' ':'0':Wsig);                                                                 
       Evalr Wano = %Char(AÑo_sys);                                                                 
       Wano = %Xlate(' ':'0':Wano);                                                                 
       WaÑo = Wsig + Wano;                                                                          
       Evalr Wmes = %Char(Mes_sys);                                                                 
       Wmes = %Xlate(' ':'0':Wmes);                                                                 
       Evalr Wdia = %Char(Dia_sys);                                                                 
       Wdia = %Xlate(' ':'0':Wdia);                                                                 
                                                                                                    
       Exec Sql                                                                                     
       Declare C2 Scroll Cursor For                                                                 
       Select Nvccia, Nvcord, Nvctpn, Nvcvlr, Nvccmt, Nvccbc, Nvcnbc,                               
              Nvcccb, Nvcncb, Nvcced                                                                
       From Votrea00/Vtwnvcf                                                                        
       Where Nvccia = :Pcia And Nvcced = :Pced1;                                                    
                                                                                                    
       Exec Sql                                                                                     
        Open C2;                                                                                    
                                                                                                    
       Exec Sql                                                                                     
        Fetch C2 Into :FieldsFile;                                                                  
                                                                                                    
       Dow Sqlcod <> 100;                                                                           
       Tsw = 'S';                                                                                   
       Kban = %Xlate(' ':'0':%Char(QNvccbc));                                                       
       Kced = Qnvcced;                                                                              
       Kord = Qnvcord;                                                                              
       Ktip = %Trim(Qnvctpn);                                                                       
       Chain (Kban:Kced:Kord:Ktip) Vtwcabf2;                                                        
       If Not %Found(Vtwcabf2);                                                                     
        Wcbced = Qnvcced;                                                                           
        Wcbtip = %Trim(Qnvctpn);                                                                    
        Wcbval = Qnvcvlr;                                                                           
        Wcbord = Qnvcord;                                                                           
        Chain(n) (%Int(Kban)) Vtmbncf;                                                              
         If %Found(Vtmbncf);                                                                        
          Wcbbco = %Trim(Bnctnd);                                                                   
         Endif;                                                                                     
        Wcbcmn = Qnvccmt;                                                                           
        Wcbfet = WaÑo + '/' + Wmes + '/' + Wdia;                                                    
        Wcbsts = *Blanks;                                                                           
        Wcbusr = User;                                                                              
        Wcbcct = Qnvcccb;                                                                           
        Write Rwcab2;                                                                               
       Endif;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Fetch C2 Into :FieldsFile;                                                                  
       Enddo;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close C2;                                                                                   
                                                                                                    
       If Tsw <> *Blanks;                                                                           
        Wvari = *Blanks;                                                                            
        Wvari = 'SBMJOB CMD(CALL PGM(VOTREP00/VTNCBI0L) PARM(' +                                    
        '''' + Pcia + '''' + '))' + ' JOB(NOVED_CART) JOBQ(QGPL/QBATCH)';                           
        Cmd = %Trim(Wvari);                                                                         
        Callp(E) Qcmdexc(Cmd:%Size(Cmd));                                                           
       Exec Sql                                                                                     
        Delete From Votrea00/Vtwnvcf Where Nvccia = :Pcia And                                       
        Nvcced = :Pced1;                                                                            
        *In68 = *On;                                                                                
        Exfmt Rdato1;                                                                               
        *In68 = *Off;                                                                               
        Cancelar = *On;                                                                             
       Endif;                                                                                       
      /End-Free                                                                                     
     PAfectarCartera   E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Consulta si este Proceso puede ser Generado    //                       
       //------------------------------------------------------------------//                       
     PPrDtaara         B                                                                            
     DPrDtaara         Pi              n                                                            
      ** Definición de Variables de Trabajo                                                         
     DProcesoW         s               n   Inz(*Off)                                                
     DCmd              s            500a   Inz(*Blanks)                                             
      /Free                                                                                         
       In *Lock MyDtaara;                                                                           
        Select;                                                                                     
         When Proceso = ' ';                                                                        
          ProcesoW = *On;                                                                           
         When Proceso <> ' ';                                                                       
          Cmd = 'SNDBRKMSG  MSG(' + '''' + 'La opción de posteo se esta ' +                         
                'ejecutado por otro usuario.. Intente más ' +                                       
                'tarde'+ '''' + ') TOMSGQ(' + %Trim(Job) + ')';                                     
          Callp(E) Qcmdexc(Cmd:%Size(Cmd));                                                         
        EndSl;                                                                                      
       Out MyDtaara;                                                                                
       Return ProcesoW;                                                                             
      /End-Free                                                                                     
     PPrDtaara         E                                                                            
