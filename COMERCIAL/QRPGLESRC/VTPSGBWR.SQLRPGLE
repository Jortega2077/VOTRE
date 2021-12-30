      **********************************************************************                        
      ** Programa     : VTPSGBWR                                          **                        
      ** Descripciòn: : Consulta de Niveles de Alas                       **                        
      ** Proyecto     : Plan Guias                                        **                        
      ** Autor        : Leonel Mauricio Parra Suarez-PersonalSoft         **                        
      ** Fecha Creaciòn: 13 de Julio de 2016                              **                        
      **********************************************************************                        
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
                                                                                                    
     ** Definiciòn de Archivo de Trabajo                                                            
                                                                                                    
     FVTPSGIWP  Cf   e             Workstn                                                          
     F                                     Sfile(RDatos1:Nrr)                                       
     F                                     Sfile(RDatos2:Nrr1)                                      
                                                                                                    
      *Definición de variables de trabajo                                                           
     DPsw              s              1a   Inz('N')                                                 
     DPosix            s              4  0 Inz(*Zeros)                                              
     DI                s              4  0 Inz(*Zeros)                                              
     DPtr              s               *   Inz(%Addr(*In))                                          
     D$coma            c                   ''''                                                     
     DPcamr            s              4a   Inz(*blanks)                                             
     DPcame            s              4a   Inz(*blanks)                                             
     Dpoper            s              1a                                                            
     Dp_cia            s              3a                                                            
     Dpnume            s              3a                                                            
     DW_PaisControl    s              3                                                             
     DW_NombrePais     s             30                                                             
     DWNombre          s             30                                                             
     DW_Cia            s              3  0                                                          
     DW_User           s             10a   Inz(*User)                                               
     DWsql             s            500a   Inz(*Blanks)                                             
     DW_Cont           S              9  0                                                          
     DW_Ind            S              1A                                                            
                                                                                                    
     DCodPais          s              3a   Inz(*Blanks)                                             
     DXcod             s              4  0 Inz(*Zeros)                                              
     DPdes             s             30a   Inz(*Blanks)                                             
     DWPiregc          s             30a   Inz(*Blanks)                                             
      **                                                                                            
     DWtitcamp         s             60a   Inz(*Blanks)                                             
     DWtitnamel        s             60a   Inz(*Blanks)                                             
     DWtitzona         s             60a   Inz(*Blanks)                                             
     DWtitmlpln        s             60a   Inz(*Blanks)                                             
     DWtitdsopr        s             60a   Inz(*Blanks)                                             
     DWtitultact       s             60a   Inz(*Blanks)                                             
     DWtitpanta0       s             60a   Inz(*Blanks)                                             
     DWtitpanta1       s             60a   Inz(*Blanks)                                             
     DWtitpanta2       s             60a   Inz(*Blanks)                                             
     DWtitsec          s             60a   Inz(*Blanks)                                             
     DWtitced          s             60a   Inz(*Blanks)                                             
     DWtitnom          s             60a   Inz(*Blanks)                                             
     DWtitpde1         s             60a   Inz(*Blanks)                                             
     DWtitala1         s             60a   Inz(*Blanks)                                             
     DWtitcal          s             60a   Inz(*Blanks)                                             
     DWtitpde2         s             60a   Inz(*Blanks)                                             
     DWtitala2         s             60a   Inz(*Blanks)                                             
     DWtitpde3         s             60a   Inz(*Blanks)                                             
     DWtitala3         s             60a   Inz(*Blanks)                                             
     DWcamp            s              4a   Inz(*Blanks)                                             
     DWnamelider       s             30a   Inz(*Blanks)                                             
     DWmailplan        s              1a   Inz(*Blanks)                                             
     DWdiasoper        s             15a   Inz(*Blanks)                                             
     DWfechaact        s             30a   Inz(*Blanks)                                             
     DWcamp1           s              4a   Inz(*Blanks)                                             
     DWcamp2           s              4a   Inz(*Blanks)                                             
     DWcamp3           s              4a   Inz(*Blanks)                                             
     DWnrosec          s              2a   Inz(*Blanks)                                             
     DWstatus          s              1a   Inz(*Blanks)                                             
     DWdestatus        s            120a   Inz(*Blanks)                                             
     DWcia             s             10a   Inz(*Blanks)                                             
     DWzona            s              3a   Inz(*Blanks)                                             
     DIdx              s              3  0 Inz(*Zeros)                                              
     DSPcia            s             10a   Inz(*Blanks)                                             
     DPres             s              2a   Inz(*Blanks)                                             
     DPciaR            s             10a   Inz(*Blanks)                                             
                                                                                                    
     ** Definiciòn de Estructuras de Trabajo                                                        
     DIndic            Ds                  Based(Ptr)                                               
     DSalir                            N   Overlay(Indic:3)                                         
     DCancelar                         N   Overlay(Indic:12)                                        
     DOcultarCmp                       N   Overlay(Indic:33)                                        
     DPosicionarZ                      N   Overlay(Indic:34)                                        
     DPNvlAlas                         N   Overlay(Indic:36)                                        
     DSflDsp                           N   Overlay(Indic:85)                                        
     DSflDspCtl                        N   Overlay(Indic:80)                                        
     DSflClr                           N   Overlay(Indic:75)                                        
     DSflEnd                           N   Overlay(Indic:70)                                        
     DMas_Inf                          N   Overlay(Indic:11)                                        
                                                                                                    
      *Definición de Vectores                                                                       
     D  VdesTpo        S             12A   DIM(5)CTDATA PERRCD(1)                                   
                                                                                                    
      *Definición de Estructuras                                                                    
     D                sDs                                                                           
     Dnomp               *Proc                                                                      
                                                                                                    
      *Definición de Procedimientos                                                                 
     DPintarfile       Pr                                                                           
     DBorrarfile       Pr                                                                           
     DPrCrear          Pr                                                                           
     DPrValidarCampos  Pr              N                                                            
     DPrActualizar     Pr                                                                           
     DPrEliminar       Pr                                                                           
                                                                                                    
      * Programas externos                                                                          
      //programa pais                                                                               
     DPgmPais          Pr                  Extpgm('VOTREP00/VTMGOI8R')                              
     DXcia                            3                                                             
     DXpais                          30A                                                            
      *Procedimiento Recupera Código de envio                                                       
     DCompaÑia         Pr                  Extpgm('VOTREP00/VTMCSBFR')                              
     DSPcia                          10                                                             
     DPres                            2                                                             
     DKciaR                          10                                                             
                                                                                                    
                                                                                                    
     DRsTramaAlas      s                   SqlType(Result_Set_Locator)                              
      *Definición de Occurs para cargue de Información                                              
     DDsNivelAlas      Ds                  Occurs(100)                                              
     DGSector                        10a                                                            
     DGCedula                         9  0                                                          
     DGNombre                        30a                                                            
     DGPedDel1                        9  0                                                          
     DGAlas1                         30a                                                            
     DGCamAlas                        2a                                                            
     DGPedDel2                        9  0                                                          
     DGAlas2                         30a                                                            
     DGPedDel3                        9  0                                                          
     DGAlas3                         30a                                                            
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //                                                                  //                        
      //            P R O G R A M A    P R I N C I P A L                  //                        
      //                                                                  //                        
      //////////////////////////////////////////////////////////////////////                        
      /Free                                                                                         
                                                                                                    
          Dow Not Salir;                                                                            
            Pfecha = %Char(%Date());                                                                
            Ppgm = Nomp;                                                                            
            PNomp = W_NombrePais;                                                                   
            PrPintarSfl();                                                                          
            If Salir;                                                                               
              Leave;                                                                                
            EndIf;                                                                                  
                                                                                                    
            If Cancelar;                                                                            
              Clear RControl1;                                                                      
              Clear W_NombrePais;                                                                   
              Clear W_PaisControl;                                                                  
            ElseIf Not Mas_Inf;  //                                                                 
              If Ppais = 1;                                                                         
                // Llama programa de servicio pais                                                  
                Pgmpais(W_PaisControl:W_NombrePais);                                                
                Ppais = 0;                                                                          
                If W_PaisControl <> *Blanks;                                                        
                  p_cia = W_PaisControl;                                                            
                  OcultarCmp=*on;                                                                   
                  PosicionarZ=*on;                                                                  
                  clear pzona;                                                                      
                Else;                                                                               
                  SflDsp = *Off;                                                                    
                  OcultarCmp=*off;                                                                  
                EndIf;                                                                              
              EndIf;                                                                                
              If OcultarCmp=*on;                                                                    
                 If pzona <> *blanks and W_PaisControl <> *Blanks;                                  
                   Clear Mas_Inf;                                                                   
                   //Se Valida Existencia de Zona                                                   
                   WZona = *Blanks;                                                                 
                   Wsql  = *Blanks;                                                                 
                   Wsql  = 'Select Smslsp From Opf' +                                               
                           %Trim(W_PaisControl) + '/PslMan ' +                                      
                           'Where SmsLsp = '+ $Coma + PZona + $Coma;                                
                                                                                                    
                   Exec Sql                                                                         
                     Prepare C0 From :Wsql;                                                         
                                                                                                    
                   Exec Sql                                                                         
                     Declare Cursor1 Cursor For C0;                                                 
                                                                                                    
                   Exec Sql                                                                         
                     Open Cursor1;                                                                  
                                                                                                    
                   Exec Sql                                                                         
                    Fetch Cursor1 Into :WZona;                                                      
                                                                                                    
                   If SqlCod = *Zeros;                                                              
                      *In38 = *Off;                                                                 
                      PNvlAlas = *On;                                                               
                   Else;                                                                            
                     *In38 = *On;                                                                   
                   EndIf;                                                                           
                                                                                                    
                   Exec Sql                                                                         
                    Close Cursor1;                                                                  
                   PintarFile();                                                                    
                   PosicionarZ=*on;                                                                 
                 Else;                                                                              
                   PosicionarZ = *on;                                                               
                 EndIf;                                                                             
              EndIf;                                                                                
            EndIf;                                                                                  
          EndDo;                                                                                    
       *Inlr = *On;                                                                                 
      /End-Free                                                                                     
                                                                                                    
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento para pintar el subfile principal                   //                       
       //------------------------------------------------------------------//                       
     PPintarfile       B                                                                            
     Dpintarfile       Pi                                                                           
      /Free                                                                                         
       Borrarfile();                                                                                
       If PosiX <> *Zeros;                                                                          
        Posic = PosiX;                                                                              
        PosiX = *Zeros;                                                                             
       Endif;                                                                                       
       clear *all DsNivelAlas;                                                                      
       SPcia = p_cia;                                                                               
       Pres = *Blanks;                                                                              
       PciaR = *Blanks;                                                                             
       CompaÑia(SPcia:Pres:PciaR);                                                                  
       WCia  =   PciaR;                                                                             
       If Pres= 'Si';                                                                               
          Exec SQL                                                                                  
            call votrep00.GSL_NIVELALAS(:WCia,:WZona,                                               
            :Wtitcamp,:Wtitnamel,:Wtitzona,:Wtitmlpln,:Wtitdsopr,                                   
            :Wtitultact,:Wtitpanta0,:Wtitpanta1,:Wtitpanta2,                                        
            :Wtitsec,:Wtitced,:Wtitnom,:Wtitpde1,:Wtitala1,:Wtitcal                                 
            ,:Wtitpde2,:Wtitala2,:Wtitpde3,:Wtitala3,:Wcamp,                                        
            :Wnamelider,:Wmailplan,:Wdiasoper,:Wfechaact,:Wcamp1,                                   
            :Wcamp2,:Wcamp3,:Wnrosec,:Wstatus,:Wdestatus);                                          
                                                                                                    
          Exec SQL                                                                                  
            Associate Result Set Locators                                                           
            (:RsTramaAlas) With Procedure Votrep00/GSL_NivelAlas;                                   
                                                                                                    
          Exec SQL                                                                                  
            Allocate Resultados Cursor                                                              
            For Result Set :RsTramaAlas;                                                            
                                                                                                    
           Exec SQL                                                                                 
             Fetch Resultados For 100 Rows Into :DsNivelAlas;                                       
                                                                                                    
          Exec SQL                                                                                  
             Close Resultados;                                                                      
                                                                                                    
          Pnroscg  = Wnrosec;                                                                       
          PNombreL = Wnamelider;                                                                    
          For idx = 1 to %elem(DsNivelAlas);                                                        
            %Occur(DsNivelAlas) = Idx;                                                              
            If DsNivelAlas.GSector <> *Blanks;                                                      
               Nrr = Nrr + 1;                                                                       
               PSECT     = DsNivelAlas.GSector;                                                     
               PCEDL     = DsNivelAlas.GCedula;                                                     
               PNOMBRE   = DsNivelAlas.GNombre;                                                     
               PPEDDEL1  = DsNivelAlas.GPedDel1;                                                    
               PALAS1    = DsNivelAlas.GAlas1;                                                      
               PCAMALAS  = DsNivelAlas.GCamAlas;                                                    
               Write RDatos1;                                                                       
               PPEDDEL2  = DsNivelAlas.GPedDel2;                                                    
               PALAS2    = DsNivelAlas.GAlas2;                                                      
               PPEDDEL3  = DsNivelAlas.GPedDel3;                                                    
               PALAS3    = DsNivelAlas.GAlas3;                                                      
               Nrr1= Nrr;                                                                           
               Write RDatos2;                                                                       
               Posi = nrr;                                                                          
            EndIf;                                                                                  
          Endfor;                                                                                   
          SflEnd = *On;                                                                             
          If Nrr > *Zeros;                                                                          
            SflDsp = *On;                                                                           
          Else;                                                                                     
            SflDsp = *Off;                                                                          
          Endif;                                                                                    
       EndIf;                                                                                       
      /End-Free                                                                                     
     PPintarfile       E                                                                            
                                                                                                    
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que se Encarga de Borrar Archivo de Control        //                       
       //------------------------------------------------------------------//                       
     PBorrarfile       B                                                                            
     DBorrarfile       Pi                                                                           
      /Free                                                                                         
       Posic = 1;                                                                                   
       Nrr = *Zeros;                                                                                
       Nrr1= *Zeros;                                                                                
       SflClr = *On;                                                                                
       Write RControl1;                                                                             
       Write RControl2;                                                                             
       SflClr = *Off;                                                                               
      /End-Free                                                                                     
     PBorrarfile       E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Pinta el Sub Archivo de la Pantalla            //                       
       //------------------------------------------------------------------//                       
     PPrPintarSfl      B                                                                            
     DPrPintarSfl      Pi                                                                           
      /Free                                                                                         
       Pfecha = %Char(%Date());                                                                     
       SflDspCtl = *On;                                                                             
       Write Teclas1;                                                                               
       If Psw = 'S' And Mas_Inf = *On;                                                              
        Psw = 'N';                                                                                  
        Mas_Inf = *Off;                                                                             
       Endif;                                                                                       
       If Mas_Inf = *Off;                                                                           
            clear PTITULO;                                                                          
            PTITULO = '                                          C' + WCAMP1 +                      
                      '       C' + WCAMP1 + '     C' + WCAMP1;                                      
            Exfmt RControl1;                                                                        
       Else;                                                                                        
        Psw = 'S';                                                                                  
        Mas_Inf = *Off;                                                                             
            clear PTITULO2;                                                                         
            PTITULO2= '                                          C' + WCAMP2 +                      
                      '   C' + WCAMP2 + '   C' + WCAMP3 + '   C' + WCAMP3;                          
            Exfmt RControl2;                                                                        
       Endif;                                                                                       
       SflDspCtl = *Off;                                                                            
      /End-Free                                                                                     
     PPrPintarSfl      E                                                                            
                                                                                                    
                                                                                                    
