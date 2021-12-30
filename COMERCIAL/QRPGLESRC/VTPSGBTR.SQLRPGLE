      **********************************************************************                        
      ** Programa     : VTPSGBTR                                          **                        
      ** Descripciòn: : Web Guías/ Detalle Inscripciones - Resultado por  **                        
      **                Campaña                                           **                        
      ** Proyecto     : Plan Guias                                        **                        
      ** Autor        : Leonel Mauricio Parra Suárez-PersonalSoft         **                        
      ** Fecha Creaciòn: 12 de Julio de 2016                              **                        
      **********************************************************************                        
     ** Definición de Directivas de Compilación                                                     
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
                                                                                                    
      ** Definición de Archivos de Trabajo                                                          
     FLcacmp01  If   e           k Disk                                                             
                                                                                                    
     FVTPSGI8P  cf   e             Workstn                                                          
     F                                     Sfile(Pdatos:Nrr)                                        
     F                                      InfDs( infDsp )                                         
     D InfDsp          DS                                                                           
     D                                     Qualified                                                
     D   key                          1A   Overlay( InfDsp: 369 )                                   
      *Definición de Procedimientos                                                                 
     DMain             Pr                  Extpgm('Main')                                           
     DIcia                            3a                                                            
     DIcamp                           4a                                                            
     DIzona                           3a                                                            
     DIsector                         1a                                                            
     DIcedulag                        9  0                                                          
     DInombreg                       30a                                                            
     DIFoco                          30a                                                            
     DIinstot                         5  0                                                          
     DIinsdel                         9  0                                                          
     DIcamins                         4a                                                            
                                                                                                    
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
     DIcia                            3a                                                            
     DIcamp                           4a                                                            
     DIzona                           3a                                                            
     DIsector                         1a                                                            
     DIcedulag                        9  0                                                          
     DInombreg                       30a                                                            
     DIFoco                          30a                                                            
     DIinstot                         5  0                                                          
     DIinsdel                         9  0                                                          
     DIcamins                         4a                                                            
                                                                                                    
      ** Definición de Variables de Trabajo                                                         
     DPtr              s               *   Inz(%Addr(*In))                                          
     DPosiX            s              5  0 Inz(*Zeros)                                              
     DSPcia            s             10a   Inz(*Blanks)                                             
     DPres             s              2a   Inz(*Blanks)                                             
     DPciaR            s             10a   Inz(*Blanks)                                             
     DIDX              s              4  0 Inz(*Zeros)                                              
     DWcia             s             10a   Inz(*Blanks)                                             
     DWzona            s              3a   Inz(*Blanks)                                             
     DWcamp            s              4a   Inz(*Blanks)                                             
     DWtitpanta0       s             60a   Inz(*Blanks)                                             
     DWtitsec          s             60a   Inz(*Blanks)                                             
     DWtitcedg         s             60a   Inz(*Blanks)                                             
     DWtitnomg         s             60a   Inz(*Blanks)                                             
     DWtitito          s             60a   Inz(*Blanks)                                             
     DWtitide          s             60a   Inz(*Blanks)                                             
     DWtitcedc         s             60a   Inz(*Blanks)                                             
     DWtitnomc         s             60a   Inz(*Blanks)                                             
     DWtitinsc         s             60a   Inz(*Blanks)                                             
     DWtitidec         s             60a   Inz(*Blanks)                                             
     DWbot01           s             30a   Inz(*Blanks)                                             
     DWbot02           s             30a   Inz(*Blanks)                                             
     DWstatus          s             10a   Inz(*Blanks)                                             
     DWdestatus        s            120a   Inz(*Blanks)                                             
                                                                                                    
     ** Definición de Estructuras de Trabajo                                                        
     DIndic            Ds                  Based(Ptr)                                               
     DSalir                            N   Overlay(Indic:3)                                         
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
                                                                                                    
     DRsTramaInscrip   s                   SqlType(Result_Set_Locator)                              
      *Definición de Occurs para cargue de Información                                              
     DDsInscripc       Ds                  Occurs(999)                                              
     DGCedula                         9  0                                                          
     DGNombre                        30a                                                            
     DGInscripcion                    1a                                                            
     DGDelta                          1a                                                            
                                                                                                    
                                                                                                    
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //                                                                  //                        
      //            P R O G R A M A    P R I N C I P A L                  //                        
      //                                                                  //                        
      //////////////////////////////////////////////////////////////////////                        
                                                                                                    
      /Free                                                                                         
       Dow Salir = *Off;                                                                            
       *in41 = *on;                                                                                 
       clear *all DsInscripc;                                                                       
       SPcia = ICia;                                                                                
       Pres = *Blanks;                                                                              
       PciaR = *Blanks;                                                                             
       CompaÑia(SPcia:Pres:PciaR);                                                                  
       WCIA  =   PciaR;                                                                             
       WZONA =   Izona;                                                                             
       WCAMP =   Icamins;                                                                           
       If Pres= 'Si';                                                                               
         Exec SQL                                                                                   
           call Votrep00.Gsl_Inscripcionesdctoguias (:Wcia,:Wzona,:Wcamp,                           
           :Isector,:Icedulag,:Inombreg,:Iinstot,:Iinsdel,:Wtitpanta0,                              
           :Wtitsec,:Wtitcedg,:Wtitnomg,:Wtitito,:Wtitide,:Wtitcedc,                                
           :Wtitnomc,:Wtitinsc,:Wtitidec,:Wbot01,:Wbot02,:Wstatus,                                  
           :Wdestatus);                                                                             
                                                                                                    
         Exec SQL                                                                                   
          Associate Result Set Locators                                                             
          (:RsTramaInscrip) With Procedure Votrep00/Gsl_Inscripcionesdctoguias;                     
                                                                                                    
         Exec SQL                                                                                   
            Allocate Resultados Cursor                                                              
            For Result Set :RsTramaInscrip;                                                         
                                                                                                    
          Exec SQL                                                                                  
           Fetch Resultados For 999 Rows Into :DsInscripc;                                          
                                                                                                    
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
     DWsql             s           3000a                                                            
     DIegcdc           s              9  0                                                          
     DMlname           s             30a                                                            
      /Free                                                                                         
       PrBorrarSfl();                                                                               
       If PosiX <> *Zeros;                                                                          
        Posic = PosiX;                                                                              
        PosiX = *Zeros;                                                                             
       Endif;                                                                                       
       PrAsigCabec();                                                                               
                                                                                                    
       Clear Wsql;                                                                                  
       Wsql = 'Drop Table Qtemp/Deta';                                                              
                                                                                                    
       Exec Sql                                                                                     
        Execute Immediate :Wsql;                                                                    
                                                                                                    
       Clear Wsql;                                                                                  
       Wsql = 'Create table Qtemp/Deta as ( ' +                                                     
              'Select * ' +                                                                         
              'From Votrea00/Vtwiegf ' +                                                            
             ' Where Iegcia = ' + '''' + %Trim(SPcia) + '''' + ' And ' +                            
              'Iegzng = ' + '''' + Wzona + '''' + ' And ' +                                         
              'Iegscg = ' + '''' + Isector + '''' + ' And ' +                                       
              'Iegcdg = ' + %Trim(%Editc(Icedulag:'Z')) + ' And ' +                                 
              'Iegcmp = ' + '''' + WCamp + '''' + ' And ' +                                         
              'Iegitp = ' + '''' + 'E' + '''' +                                                     
              ') With Data';                                                                        
                                                                                                    
       Exec Sql                                                                                     
        Execute Immediate :Wsql;                                                                    
                                                                                                    
       For idx = 1 to %Elem(DsInscripc);                                                            
        %Occur(DsInscripc)= Idx;                                                                    
                                                                                                    
                                                                                                    
        IF GNombre <> *Blanks Or GInscripcion <> *Blanks                                            
           Or GCedula <> *Zeros Or GDelta <> *Blanks;                                               
                                                                                                    
        Clear Wsql;                                                                                 
        Wsql = 'Delete From Qtemp/Deta ' +                                                          
               ' Where Iegcia = ' + '''' + %Trim(SPcia) + '''' + ' And ' +                          
               'Iegzng = ' + '''' + Wzona + '''' + ' And ' +                                        
               'Iegscg = ' + '''' + Isector + '''' + ' And ' +                                      
               'Iegcdc = ' + %Trim(%Editc(GCedula:'Z')) + ' And ' +                                 
               'Iegcmp = ' + '''' + WCamp + '''';                                                   
                                                                                                    
        Exec Sql                                                                                    
         Execute Immediate :Wsql;                                                                   
                                                                                                    
         Nrr = Nrr +1;                                                                              
           PCD = GCedula;                                                                           
           PNOMBRE  = GNombre;                                                                      
          IF GInscripcion = ' ';                                                                    
           PINSCRIP = '-';                                                                          
          Else;                                                                                     
           PINSCRIP = GInscripcion;                                                                 
          Endif;                                                                                    
          If GDelta = ' ';                                                                          
           PDELTA   = '-';                                                                          
          Else;                                                                                     
           PDELTA   =  GDelta;                                                                      
          EndIf;                                                                                    
          Posi = Nrr;                                                                               
          Write PDatos;                                                                             
        EndIF;                                                                                      
       Endfor;                                                                                      
                                                                                                    
       Clear Wsql;                                                                                  
       Wsql = 'Select Iegcdc, Mlname ' +                                                            
              'From Qtemp/Deta Inner Join Opf' + %Trim(SPcia) +                                     
              '/Lmllst01 On Iegcdc = MlcstÑ ' +                                                     
              'Where Iegcdg = ' + %Trim(%Editc(Icedulag:'Z'));                                      
                                                                                                    
       Exec Sql                                                                                     
        Prepare C1 From :Wsql;                                                                      
                                                                                                    
       Exec Sql                                                                                     
        Declare Cur1 Cursor For C1;                                                                 
                                                                                                    
       Exec Sql                                                                                     
        Open Cur1;                                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cur1 Into :Iegcdc, :Mlname;                                                           
                                                                                                    
       Dow SqlCod <> 100 And SqlCod >= *Zeros;                                                      
         Nrr = Nrr +1;                                                                              
         PCD = Iegcdc;                                                                              
         PNOMBRE  = Mlname;                                                                         
         PINSCRIP = 'X';                                                                            
         PDELTA   = 'X';                                                                            
         Posi = Nrr;                                                                                
         Write PDatos;                                                                              
        Exec Sql                                                                                    
         Fetch Cur1 Into :Iegcdc, :Mlname;                                                          
       Enddo;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close Cur1;                                                                                 
                                                                                                    
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
        Pcampc  = 'Ciere a C'+ ICamp;                                                               
        Pzona   = Wzona;                                                                            
        Psectc  = Isector;                                                                          
        PCEDUG  = Icedulag;                                                                         
        PNOMBG  = Inombreg;                                                                         
        Pfoco   = IFoco;                                                                            
        PCÑA1   = 'C' + Icamins;                                                                    
        PCÑA2   = 'C' + Icamins;                                                                    
        If Iinstot = *Zeros;                                                                        
         PTTINSDLT= '#0';                                                                           
        Else;                                                                                       
         PTTINSDLT= '#' + %Trim(%editc(Iinstot:'Z'));                                               
        EndIf;                                                                                      
        If Iinsdel = *Zeros;                                                                        
         PINSDLT =  '#0';                                                                           
        Else;                                                                                       
         PINSDLT =  '#' + %Trim(%editc(Iinsdel:'Z'));                                               
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
       Write  teclas1;                                                                              
       Exfmt Pcontrol;                                                                              
       SflDspCtl = *Off;                                                                            
      /End-Free                                                                                     
     PPrPintarSfl      E                                                                            
