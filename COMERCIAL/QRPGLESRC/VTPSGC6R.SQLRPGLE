      ***********************************************************************                       
      ** Programa      : VTPSGC6R                                          **                       
      ** Descripciòn   : PGM Consulta Control de Pagos Tradicional         **                       
      ** Proyecto      : Plan Lideres Tradicional                          **                       
      ** Autor         : (AparraQ)                                         **                       
      ** Fecha Creación: Enero de 2018                                     **                       
      ***********************************************************************                       
     HDftactgrp(*No)                                                                                
     HOption(*NoDebugIo)                                                                            
     Hdatfmt(*Iso)                                                                                  
                                                                                                    
     **Definicion de informes                                                                       
     FVTPSGC0P  Cf   E             Workstn InfDs( infDsp )                                          
     FVTMSMPF   IF   E           K Disk                                                             
     FVTWCGTF   Uf   E             Disk    Rename(VTWCGTF:RegWCGT) Usropn                           
                                                                                                    
                                                                                                    
     ** Definiciòn de Estructuras de Trabajo                                                        
     D                sDs                                                                           
     Dnomp               *Proc                                                                      
                                                                                                    
     D InfDsp          DS                                                                           
     D                                     Qualified                                                
     D   key                          1A   Overlay( InfDsp: 369 )                                   
      *                                                                                             
     DFields         e Ds                  Extname(VtwLpgf:RegLpg)                                  
     D                                     Prefix(Q)                                                
      *                                                                                             
                                                                                                    
     DFieldsC        e Ds                  Extname(PARDTL:PARDTLF1)                                 
     D                                     Prefix(Q)                                                
                                                                                                    
                                                                                                    
     DResulSetZon      Ds                  Occurs(999) Qualified                                    
     DSelec                           1a                                                            
     DW_ZONA                          3a                                                            
                                                                                                    
      **Estructura para los indicadores de pantalla                                                 
     Dptr              S               *   Inz(%addr(*In))                                          
     D@ind             DS                  Based(ptr)                                               
     D F3Salir                         N   Overlay(@Ind:03)                                         
     D F5Generar                       N   Overlay(@Ind:05)                                         
     D F12Cancelar                     N   Overlay(@Ind:12)                                         
     D PosicionarCurP                  N   Overlay(@Ind:30)                                         
     D PosicionarCurC                  N   Overlay(@Ind:31)                                         
     D PosicionarCurZ                  N   Overlay(@Ind:32)                                         
                                                                                                    
      **Definición de Procedimientos                                                                
     DPrCreateFile     Pr                                                                           
     DPrReprocesar     Pr                                                                           
     DPrUpdateP        Pr                                                                           
     DPrDspFile        Pr                                                                           
     DValidaCamps      Pr             1N                                                            
     DPrCentrar        Pr            50A                                                            
     D pCadena                       50A   Const                                                    
                                                                                                    
       // Definicion Prototipo para un Comando Externo QCMDEXC.                                     
     D RunSysCmd       Pr                  ExtPgm('QCMDEXC')                                        
     D Cmd                           20A   Options(*VarSize) Const                                  
     D CmdLen                        15P 5 Const                                                    
                                                                                                    
     DPgmPais1         Pr                  Extpgm('VOTREP00/VTMGOI8R')                              
     DXcia                            3                                                             
     Dwpais                          30                                                             
                                                                                                    
     DPgmCampaÑa       Pr                  Extpgm('VOTREP00/VTMGOIFR')                              
     DXcia                            3a                                                            
     DXCamp                           4a                                                            
     DXInd                            1A                                                            
                                                                                                    
     Dpgmzona          Pr                  Extpgm('VOTREP00/VTMGOINR')                              
     DCia                             3                                                             
     DICam                            4                                                             
     DNumZon                          3  0                                                          
     Drespuesta                      30                                                             
     DEstrucZna                            LikeDs(ResulSetZon)                                      
                                                                                                    
     DpgmReproceso     Pr                  Extpgm('VOTREP00/VTPSGC2L')                              
     DCia                             3                                                             
     DIZon                            3                                                             
     DICam                            4                                                             
                                                                                                    
     DPgmValoresN      Pr                  Extpgm('VOTREP00/VTMGOIQR')                              
     DWADTNDÑ                        30                                                             
                                                                                                    
     DConversionID     Pr                  Extpgm('VOTREP00/VTMCSBCR')                              
     DTcia                            3                                                             
     DTced                           30                                                             
     DTcns                            9                                                             
                                                                                                    
      **Definición de Variables de Trabajo                                                          
     DUser             s             10a   Inz(*User)                                               
     DSqlStr           S           2000a   Inz(*Blanks) Varying                                     
     D$C               c                   ''''                                                     
     DXInd             s              1A                                                            
     Dwpais            s             30                                                             
     DICam             s              4                                                             
     DIZon             s              3                                                             
     DNumZon           s              3  0 Inz(*Zeros)                                              
     Drespuesta        s             30                                                             
     Dw_Moneda         S              5A   Inz(*Blanks)                                             
     DwadtndÑ          s             30    Inz(*Blanks)                                             
     DTced             S             30A   Inz(*Blanks)                                             
     DTcns             S              9a   Inz(*Blanks)                                             
                                                                                                    
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //            P R O G R A M A    P R I N C I P A L                  //                        
      //////////////////////////////////////////////////////////////////////                        
      /free                                                                                         
                                                                                                    
       Dow Not F3Salir;                                                                             
           PTitulo = PrCentrar('Consulta Control de Pagos Tradicional');                            
           Clear Blanco;                                                                            
           Write Blanco;                                                                            
           Exfmt principal;                                                                         
           Clear Perror;                                                                            
           If POPPPAIS=1;                                                                           
              Clear POPPPAIS;                                                                       
              compania = *Blanks;                                                                   
              PgmPais1(compania:wpais);                                                             
              PDESPAI = wpais;                                                                      
              If PDESPAI = *Blanks;                                                                 
                 PosicionarCurP=*On;                                                                
              Else;                                                                                 
                 PosicionarCurP=*Off;                                                               
                 PosicionarCurC=*On;                                                                
              EndIf;                                                                                
              Clear PDESCAMP;                                                                       
              Clear PDESZONA;                                                                       
              Iter;                                                                                 
           EndIf;                                                                                   
           If PPCAMPAÑA = 1;                                                                        
              Clear PPCAMPAÑA;                                                                      
              Clear PDESCAMP;                                                                       
              Clear PDESZONA;                                                                       
              Callp PgmCampaÑa(compania:PDESCAMP:XInd);                                             
              If PDESCAMP = *Blanks;                                                                
                 PosicionarCurC=*On;                                                                
              Else;                                                                                 
                 PosicionarCurC=*Off;                                                               
                 PosicionarCurZ=*On;                                                                
              EndIf;                                                                                
              Iter;                                                                                 
           EndIf;                                                                                   
           If PPZONA = 1;                                                                           
              Clear PPZONA;                                                                         
              Clear PDESZONA;                                                                       
              Clear NumZon;                                                                         
              clear respuesta;                                                                      
              Clear *all ResulSetZon;                                                               
              ICam= %trim(PDESCAMP);                                                                
              Callp(e) PgmZona(compania:ICam:NumZon:respuesta:ResulSetZon);                         
              If NumZon = 1;                                                                        
                 %Occur(ResulSetZon)= NumZon;                                                       
                 PDESZONA = ResulSetZon.W_ZONA;                                                     
              elseIf %Trim(respuesta) = 'TODAS LAS ZONAS';                                          
                 PDESZONA = respuesta;                                                              
              elseIf NumZon > 1;                                                                    
                 PDESZONA = 'VARIAS ZONAS';                                                         
              EndIf;                                                                                
              If PDESZONA = *Blanks;                                                                
                 PosicionarCurZ=*On;                                                                
              Else;                                                                                 
                 PosicionarCurZ= *Off;                                                              
                 PosicionarCurP =*On;                                                               
              EndIf;                                                                                
              Iter;                                                                                 
           EndIF;                                                                                   
           If F3Salir;                                                                              
              Leave;                                                                                
           EndIf;                                                                                   
           If F12Cancelar;                                                                          
              Clear principal;                                                                      
              F12Cancelar = *Off;                                                                   
              Iter;                                                                                 
           EndIf;                                                                                   
           If F5Generar;                                                                            
              If ValidaCamps = *Off;                                                                
                    PrReprocesar();                                                                 
                    PrCreateFile();                                                                 
                    PrUpdateP();                                                                    
                    PrDspFile();                                                                    
                    PMSJ1='    La Consulta se ha generado Exitosamente!!';                          
                    Pfile = 'VTWCGTF';                                                              
                    Plib = User;                                                                    
                    Write Blanco;                                                                   
                    exfmt mensaje;                                                                  
                    Clear principal;                                                                
                    Iter;                                                                           
              EndIf;                                                                                
              F5Generar = *Off;                                                                     
           EndIf;                                                                                   
         If InfDsp.key = X'F1';                                                                     
           If ValidaCamps = *Off;                                                                   
              Iter;                                                                                 
           EndIf;                                                                                   
           EndIf;                                                                                   
       EndDo;                                                                                       
        *inlr=*on;                                                                                  
      /End-Free                                                                                     
                                                                                                    
                                                                                                    
       //----------------------------------------------------------------------//                   
       // Procedimiento de validacion de campos de entrada                     //                   
       //----------------------------------------------------------------------//                   
     PValidaCamps      B                                                                            
     DValidaCamps      Pi             1N                                                            
      *Variables locales                                                                            
     DIndCam           S              1N   Inz(*Off)                                                
     Dw_Cont           S              5s 0 Inz(*Zeros)                                              
      /Free                                                                                         
                                                                                                    
        PosicionarCurP=*Off;                                                                        
        PosicionarCurC=*Off;                                                                        
        PosicionarCurZ=*Off;                                                                        
        Clear Perror;                                                                               
        If PDESPAI = *Blanks;                                                                       
           PosicionarCurP=*On;                                                                      
           Perror = 'Ingrese la compañia';                                                          
           IndCam = *On;                                                                            
           Return IndCam;                                                                           
        EndIf;                                                                                      
                                                                                                    
        If PDESCAMP=*Blanks;                                                                        
           PosicionarCurC = *On;                                                                    
           Perror = 'Seleccione Campaña';                                                           
           IndCam = *On;                                                                            
           Return IndCam;                                                                           
        EndIf;                                                                                      
                                                                                                    
        If PDESZONA=*Blanks;                                                                        
           PosicionarCurZ = *On;                                                                    
           Perror = 'Seleccione Zona';                                                              
           IndCam = *On;                                                                            
           Return IndCam;                                                                           
        EndIf;                                                                                      
                                                                                                    
        //Exec Sql                                                                                  
        // Select Count(1) Into :w_Cont                                                             
        // From Votrea00/VTHCGTF                                                                    
        // Where CGTCIA = :compania                                                                 
        //   And CGTMED = :ICam;                                                                    
        //If w_Cont = 0;                                                                            
        //   PosicionarCurC = *On;                                                                  
        //   Perror = 'No hay datos para la Campaña seleccionada';                                  
        //   IndCam = *On;                                                                          
        //   Return IndCam;                                                                         
        //EndIf;                                                                                    
                                                                                                    
       Return IndCam;                                                                               
                                                                                                    
      /End-Free                                                                                     
     PValidaCamps      E                                                                            
                                                                                                    
                                                                                                    
       //----------------------------------------------------------------------//                   
       // Procedimiento de Reprocesa Zonas a Consultar                         //                   
       //----------------------------------------------------------------------//                   
     PPrReprocesar     B                                                                            
      *Variables locales                                                                            
     Ddx1              s              3  0 Inz(*Zeros)                                              
      /Free                                                                                         
                                                                                                    
        If PDESZONA <> *Blanks;                                                                     
           For dx1 = 1 to NumZon;                                                                   
               %OCCUR(ResulSetZon) = dx1;                                                           
               pgmReproceso(Compania:ResulSetZon.W_ZONA:ICam);                                      
           EndFor;                                                                                  
        EndIf;                                                                                      
                                                                                                    
      /End-Free                                                                                     
     PPrReprocesar     E                                                                            
                                                                                                    
                                                                                                    
                                                                                                    
       //----------------------------------------------------------------------//                   
       // Procedimiento de validacion de campos de entrada                     //                   
       //----------------------------------------------------------------------//                   
     PPrCreateFile     B                                                                            
      *Variables locales                                                                            
     Ddx1              s              3  0 Inz(*Zeros)                                              
      /Free                                                                                         
                                                                                                    
        //Creación de tabla de Usuario                                                              
        SqlStr = 'Drop Table ' + %Trim(User) + '/VTWCGTF';                                          
        Exec Sql                                                                                    
         Execute Immediate :SqlStr;                                                                 
                                                                                                    
        SqlStr = 'Create Table ' + %Trim(User) + '/VTWCGTF ' +                                      
                 'Like Votrea00/VTHCGTF';                                                           
        Exec Sql                                                                                    
         Execute Immediate :SqlStr;                                                                 
                                                                                                    
        SqlStr = 'Label on Table ' + %Trim(User) + '/VTWCGTF Is ' +                                 
                 '''Informe Control Pagos Tradicional''';                                           
        Exec Sql                                                                                    
         Execute Immediate :SqlStr;                                                                 
                                                                                                    
        SqlStr = 'Delete From ' + %Trim(User) + '/VTWCGTF';                                         
        Exec Sql                                                                                    
         Execute Immediate :SqlStr;                                                                 
                                                                                                    
        //Inserción de datos a archivo de Usuario                                                   
        SqlStr = 'Insert Into ' + %Trim(User) + '/VTWCGTF (' +                                      
                 'Select * '+                                                                       
                 'From Votrea00/VTHCGTF ' +                                                         
                 'Where CGTCIA = ' +$C+ Compania +$C+ ' And ' +                                     
                 'CGTMED = ' +$C+ ICam +$C;                                                         
                                                                                                    
        If PDESZONA <> *Blanks;                                                                     
           SqlStr = %trim(SqlStr) + ' And CGTZON In (';                                             
           For dx1 = 1 to NumZon;                                                                   
               %OCCUR(ResulSetZon) = dx1;                                                           
               SqlStr = %trim(SqlStr) +$C+ ResulSetZon.W_ZONA +$C;                                  
               If dx1 < NumZon ;                                                                    
                  SqlStr = ' '+%trim(SqlStr) + ' , ';                                               
               EndIf;                                                                               
           EndFor;                                                                                  
           SqlStr = ' ' + %trim(SqlStr) + ')';                                                      
        EndIf;                                                                                      
        SqlStr = %trim(SqlStr) + ')' +                                                              
                 ' Order By CGTCIA, CGTMED, CGTREG, CGTZON';                                        
        Exec Sql                                                                                    
         Execute Immediate :SqlStr;                                                                 
                                                                                                    
      /End-Free                                                                                     
     PPrCreateFile     E                                                                            
                                                                                                    
                                                                                                    
       //----------------------------------------------------------------------//                   
       // Procedimiento para actualiza pagos (Cartera - Efectivo)              //                   
       //----------------------------------------------------------------------//                   
     PPrUpdateP        B                                                                            
      *Variables de Trabajo                                                                         
     DCmdData          S            100A                                                            
      /Free                                                                                         
                                                                                                    
        CmdData = 'DLTOVR FILE(VTWCGTF PARDTL)';                                                    
        CallP(E) RunSysCmd(CmdData: %Size(CmdData));                                                
        CmdData = 'OVRDBF FILE(VTWCGTF) TOFILE(' +%Trim(User)+ '/VTWCGTF)';                         
        CallP(E) RunSysCmd(CmdData: %Size(CmdData));                                                
        CmdData = 'OVRDBF FILE(PARDTL) TOFILE(OPF' +%Trim(Compania)+ '/PARDTL)';                    
        CallP(E) RunSysCmd(CmdData: %Size(CmdData));                                                
                                                                                                    
        //Consulta simbolo de Moneda                                                                
        Chain (Compania) VTMSMPF;                                                                   
        If %Found(VTMSMPF);                                                                         
           w_Moneda = SMPSIG;                                                                       
        EndIf;                                                                                      
                                                                                                    
        If Not %open(VTWCGTF);                                                                      
           Open VTWCGTF;                                                                            
        EndIf;                                                                                      
                                                                                                    
        Setll *Start VTWCGTF;                                                                       
        Read VTWCGTF;                                                                               
        Dow Not %Eof(VTWCGTF);                                                                      
            PrCarteraEfec();                                                                        
            Read VTWCGTF;                                                                           
        EndDo;                                                                                      
                                                                                                    
        If %Open(VTWCGTF);                                                                          
           Close VTWCGTF;                                                                           
        EndIf;                                                                                      
                                                                                                    
      /End-Free                                                                                     
     PPrUpdateP        E                                                                            
                                                                                                    
                                                                                                    
       //----------------------------------------------------------------------//                   
       // Procedimiento para actualiza pagos (Cartera - Efectivo)              //                   
       //----------------------------------------------------------------------//                   
     PPrCarteraEfec    B                                                                            
      *Variables de Trabajo                                                                         
     Dw_FechaEfe       s               D                                                            
     Dw_ValorEfe       s             14  0 Inz(*Zeros)                                              
     Dw_EstEfe         s              1A   Inz(*Blanks)                                             
     Dw_campana        s              4A   Inz(*Blanks)                                             
     Dw_WADTNDÑ        s              4A   Inz(*Blanks)                                             
     Dw_Cedula         s             15S 0 Inz(*Zeros)                                              
     Dw_VlrRete        s             11S 2 Inz(*Zeros)                                              
     Dw_SearchC        c                   'Dcto Cial Cond'                                         
     Dw_SearchR        c                   'DctoCdLP'                                               
     Dw_SearchE        c                   'Dt Cd Líder Ef'                                         
      *(1)pago Cartera, (2)pago Efectivo                                                            
     DV_pago           s             15  2 Dim(4)                                                   
     DV_fecha          s             10a   Dim(4)                                                   
      *                                                                                             
     D                 Ds                                                                           
     D DsFecha                       10                                                             
     D   wDDFec                       2  0 OVERLAY(DsFecha:1) Inz(*Zeros)                           
     D   Separador1                   1    OVERLAY(DsFecha:3) Inz('-')                              
     D   WMMFec                       2  0 OVERLAY(DsFecha:4) Inz(*Zeros)                           
     D   Separador2                   1    OVERLAY(DsFecha:6) Inz('-')                              
     D   WCCFec                       2  0 OVERLAY(DsFecha:7) Inz(*Zeros)                           
     D   WYYFec                       2  0 OVERLAY(DsFecha:9) Inz(*Zeros)                           
                                                                                                    
     D                 ds                                                                           
     DDsNPLcmp1                1      4                                                             
     DDsNPLcmp2                5      8                                                             
     DdsCampanaNL              1      8                                                             
      /Free                                                                                         
                                                                                                    
        //Se obtiene la Cedula de la guia mediante la rutina VTMCSBCR                               
        If CGTCOD >= 995000000;                                                                     
           Clear Tced;                                                                              
           Tcns = %Trim(%Char(CGTCOD));                                                             
           ConversionID(Compania:Tced:Tcns);                                                        
           Monitor;                                                                                 
            w_Cedula = %Int(%Trim(Tced));                                                           
           On-Error;                                                                                
            w_Cedula = CGTCOD;                                                                      
           EndMon;                                                                                  
        Else;                                                                                       
            w_Cedula = CGTCOD;                                                                      
        EndIf;                                                                                      
                                                                                                    
                                                                                                    
        w_campana = CGTMED;                                                                         
        Clear V_pago;                                                                               
        Clear V_fecha;                                                                              
        Clear FieldsC;                                                                              
        Exec sql                                                                                    
         Declare CursorC cursor for                                                                 
         Select *                                                                                   
         From PARDTL                                                                                
         Where AdcstÑ = :CGTCOD;                                                                    
        Exec Sql                                                                                    
         Open CursorC;                                                                              
        Exec Sql                                                                                    
         fetch CursorC Into :FieldsC;                                                               
        Dow SqlCod = *Zeros;                                                                        
            Clear wadtndÑ;                                                                          
            WadtndÑ  = QadtndÑ;                                                                     
            PgmValoresN(wadtndÑ);                                                                   
            Clear dsCampanaNL;                                                                      
            dsCampanaNL =%Trim(wadtndÑ);                                                            
            W_WadtndÑ =  DsNPLcmp1;                                                                 
            If w_WadtndÑ = w_campana;                                                               
               If Qadtram < *zeros;                                                                 
                  Qadtram = Qadtram * -1;                                                           
                  IF Qadrdpt= 42;                                                                   
                     If %Scan(w_SearchC:QadtndÑ) > 0;                                               
                        // Or %Scan(w_SearchR:QadtndÑ) > 0;                                         
                        V_pago(1) += Qadtram;                                                       
                        WCCFec = QadtrCC;                                                           
                        WYYFec = QadtrYY;                                                           
                        WMMFec = QadtrMM;                                                           
                        wDDFec = QadtrDD;                                                           
                        V_fecha(1) = DsFecha;                                                       
                     EndIf;                                                                         
                  ElseIF Qadrdpt= 43;                                                               
                     If %Scan(w_SearchE:QadtndÑ) > 0;                                               
                        V_pago(2) += Qadtram;                                                       
                        WCCFec = QadtrCC;                                                           
                        WYYFec = QadtrYY;                                                           
                        WMMFec = QadtrMM;                                                           
                        wDDFec = QadtrDD;                                                           
                        V_fecha(2) = DsFecha;                                                       
                     EndIf;                                                                         
                  EndIf;                                                                            
               EndIf;                                                                               
            EndIf;                                                                                  
            Exec Sql                                                                                
             fetch CursorC Into :FieldsC;                                                           
        EndDo;                                                                                      
        Exec Sql                                                                                    
         Close CursorC;                                                                             
                                                                                                    
        If V_pago(1) <> 0;                                                                          
           CGTIAC = 'X';                                                                            
           CGTVAC = %Trim(w_Moneda) + ' '                                                           
                    +%Trim(%Editc(V_pago(1):'K'));                                                  
           CGTFEC = V_fecha(1);                                                                     
           If V_pago(1) = CGTDSC And CGTFEE = '';                                                   
              Clear CGTIPE;                                                                         
           EndIf;                                                                                   
        EndIf;                                                                                      
                                                                                                    
        //Efectivo                                                                                  
        Clear w_FechaEfe;                                                                           
        Exec Sql                                                                                    
         Select Sum(PGEVLP), Max(PGEFEC), Min(PGEEST)                                               
         Into :w_ValorEfe, :w_FechaEfe, :w_EstEfe                                                   
         From  VOTREA00/VTWPGEF                                                                     
         Where PGECIA = :CGTCIA                                                                     
           And PGECMP = :CGTMED                                                                     
           And PGECDG = :w_Cedula;                                                                  
                                                                                                    
        If w_EstEfe <> '';                                                                          
           CGTIPE = 'X';                                                                            
           If w_EstEfe = 'R' Or w_EstEfe = 'V';                                                     
              CGTVPE = %Trim(w_Moneda) + ' '                                                        
                       +%Trim(%Editc(w_ValorEfe:'K'));                                              
              CGTFEE = %Char(w_FechaEfe);                                                           
              If w_ValorEfe = CGTDSC And CGTFEC = '';                                               
                 Clear CGTIAC;                                                                      
              EndIf;                                                                                
           EndIf;                                                                                   
           If V_pago(2) = 0;                                                                        
              Clear CGTIPE;                                                                         
              Clear CGTVPE;                                                                         
              Clear CGTFEE;                                                                         
           EndIf;                                                                                   
        EndIf;                                                                                      
                                                                                                    
        Exec Sql                                                                                    
         Select Sum(VRFVRR) into :w_VlrRete                                                         
         From Votrea00/Vtwvrff                                                                      
         Where Vrfcia =:CGTCIA                                                                      
         And Vrfcam =:CGTMED                                                                        
         And Vrfced =:CGTCOD                                                                        
         And Vrfipc = 'P';                                                                          
        If w_VlrRete <> 0;                                                                          
           CGTVRT = %Trim(w_Moneda) + ' '                                                           
                    +%Trim(%Editc(w_VlrRete:'K'));                                                  
        EndIf;                                                                                      
                                                                                                    
        If w_ValorEfe <> 0 Or V_pago(1) <> 0;                                                       
           Update RegWCGT %Fields(CGTIAC:CGTVAC:CGTFEC:CGTIPE:CGTVPE:                               
                                  CGTFEE:CGTVRT);                                                   
        EndIf;                                                                                      
                                                                                                    
      /End-Free                                                                                     
     PPrCarteraEfec    E                                                                            
                                                                                                    
                                                                                                    
       //----------------------------------------------------------------------//                   
       // Procedimiento para visualizar archivo de usuario                     //                   
       //----------------------------------------------------------------------//                   
     PPrDspFile        B                                                                            
      *Variables de Trabajo                                                                         
     DCmdData          S            100A                                                            
      /Free                                                                                         
                                                                                                    
       CmdData = 'Verdes ' + %Trim(User) + '/VTWCGTF';                                              
       CallP(E) RunSysCmd(CmdData: %Size(CmdData));                                                 
                                                                                                    
      /End-Free                                                                                     
     PPrDspFile        E                                                                            
                                                                                                    
                                                                                                    
      *------------------------------------------------------------------------                     
      * Centrar cadena alfanumérica                                                                 
      *------------------------------------------------------------------------                     
     PPrCentrar        B                                                                            
     D                 Pi            50A                                                            
     D pCadena                       50A   Const                                                    
                                                                                                    
      *- Variables                                                                                  
     DW_Blanco         S              5S 0                                                          
     DW_Inicio         S              5S 0 Inz(*Zeros)                                              
     DW_Longit         S              5S 0 Inz(*Zeros)                                              
     DW_TxtCen         S             50A   Inz(*Blanks)                                             
      /Free                                                                                         
       //Validar mensaje                                                                            
         If (pCadena = *Blanks) Or                                                                  
            (%Len(%Trim(pCadena)) = 50);                                                            
             Return pCadena;                                                                        
         EndIf;                                                                                     
                                                                                                    
       //Calcular longitud de Texto y ubicarlo en el centro                                         
         W_Blanco = %Len(pCadena) - %Len(%Trim(pCadena));                                           
         W_Inicio = (W_Blanco/2) + 1;                                                               
         W_Longit = %Len(%Trim(pCadena));                                                           
         %SubSt(W_TxtCen:W_Inicio:W_Longit) = %Trim(pCadena);                                       
       //Devolver Texto Centrado                                                                    
         Return W_TxtCen;                                                                           
      /End-Free                                                                                     
     PPrCentrar        E                                                                            
