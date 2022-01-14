***************************************************************************                         
     **                                                                   *                         
     ** Programa      : VTMCC23R                                          *                         
     ** Descripciòn:  : Lee Archivo Enviado por el Banco                  *                         
     ** Proyecto      : Menú de Opciones Opi Paises                       *                         
     ** Autor         : Leonel Mauricio Parra Suárez - PersonalSoft       *                         
     ** Autor         : (Jortega)                                         *                         
     ** Fecha Creaciòn: 18 de Octubre de 2018                             *                         
     **                                                                   *                         
     **********************************************************************                         
     ** Definición de Directivas de Compilación                                                     
     H*Option(*NoDebugIO) Dftactgrp(*No)                                                            
     H Bnddir('HTTPAPI':'QC2LE')                                                                    
     H Timfmt(*Hms)                                                                                 
     H Option(*NoDebugIO: *ShowCpy:*NoXRef:*NoExpDds:*NoSecLvl:*NoExt:*SrcStMt)                     
     H DatFmt(*Iso) Aut(*all)                                                                       
                                                                                                    
                                                                                                    
       /Define CountUsingSql                                                                        
       /Include Votrep00/Qrpglesrc,Vtmgoi14r                                                        
       /Undefine CountUsingSql                                                                      
                                                                                                    
     ** Definición de Archivos de Trabajo                                                           
     FVttbcpf   If   e           k Disk                                                             
     FVttbcsf   If   e           k Disk                                                             
     FVttrbsf   Uf   e           k Disk                                                             
     FVtmbncf   If   e           k Disk                                                             
     FLmllst01  If   e           k Disk                                                             
                                                                                                    
     ** Definiciòn de Archivo de Pantallas                                                          
                                                                                                    
     ** Definiciòn de Variables de Trabajo                                                          
     DSearchString     s           1000                                                             
     DCount            s             10p 0                                                          
     D $C              c                   ''''                                                     
     DDir              s               *                                                            
     DP_Dirent         s               *                                                            
     DFile             s            500a   Inz(*Blanks)                                             
     DFileX            s            500a   Inz(*Blanks)                                             
     DRp               s             10i 0 Inz(*Blanks)                                             
     DNumsep           s              4s 0 Inz(*Zeros)                                              
     DNroSepF          s              4s 0 Inz(*Zeros)                                              
     DDatos            s              1a   Inz(*Blanks)                                             
     DDatosR           s           1000a   Varying                                                  
     D wSql            s          11000a                                                            
     DW_encabezado     s           1000a   Varying                                                  
     DRuta             s             70a   Inz(*Blanks)                                             
     DPos1             s              3s 0 Inz(*Zeros)                                              
     DRowCount         s              4s 0 Inz(*Zeros)                                              
     DSqlNr1           s            500a   Inz(*Blanks)                                             
     DSqlNr2           s            500a   Inz(*Blanks)                                             
     DSqlNr6           s            500a   Inz(*Blanks)                                             
     DSep              s              1a   Inz(*Blanks)                                             
     DVecPos           s           1000a   Inz(*Blanks)                                             
     DVecPosY          s           1000a   Inz(*Blanks)                                             
     DPcdbax           s              2s 0 Inz(*Zeros)                                              
     DPcdesx           s              3s 0 Inz(*Zeros)                                              
     DXreg             s           1000    Inz(*Zeros)                                              
     DIdValor          s             11s 2 Inz(*Zeros)                                              
     Dw_numero         s             16s 4 Inz(*Zeros)                                              
     DIdValora         s             50    Inz(*Blanks)                                             
     DIdfec            s             10a   Inz(*Blanks)                                             
     DRfile            s             10a   Inz(*Blanks)                                             
     DSqlNr3           s           2000a   Inz(*Blanks)                                             
     DTotalRecs        s              7s 0 Inz(*Zeros)                                              
     DPsw              s              1a   Inz(*Blanks)                                             
     DODecima          s              4s 0 Inz(*Zeros)                                              
     DRbsnarW          s                   like(Rbsnar) Inz(*Blanks)                                
     DRbsfcaW          s             10a   Inz(*Blanks)                                             
     DRbshcaW          s             10a   Inz(*Blanks)                                             
     DYfile            s             10a   Inz(*Blanks)                                             
     DPNomFile         s             20a   Inz(*Blanks)                                             
     DDetalleP         s           1000a   Inz(*Blanks)                                             
     DPnomBanco        s             40a   Inz(*Blanks)                                             
     DPnomEstru        s             30a   Inz(*Blanks)                                             
     Dxfile            s             20a   Inz(*Blanks)                                             
     D*W_cuenta         s              2  0 Inz(*Zeros)                                             
     DW_sql            s           3000a   Inz(*Blanks)                                             
     DPos3             s              3s 0 Inz(*Zeros)                                              
     DPos5             s              3s 0 Inz(*Zeros)                                              
     DPos6             s              9s 0 Inz(*Zeros)                                              
     DW_P1CED          s             30a   Inz(*Blanks)                                             
     DW_P1CEDAux       s           1000a   Inz(*Blanks)                                             
     DW_P1CEDAux1      s             30a   Inz(*Blanks)                                             
     DPcedulaa         s           1000a   Inz(*Blanks)                                             
     DPcedulaaB        s             30  0                                                          
     D*w_cdt            s                   Likeds(QAsbCdt)                                         
     D*w_cen            s                   Likeds(QAsbcen)                                         
     D DX1             s              3s 0 Inz(*Zeros)                                              
     D WDX1            s              2s 0 Inz(*Zeros)                                              
     D W_FILETEMP      s             30a   Inz(*Blanks)                                             
     DKKsw             s               n   Inz(*Off)                                                
       Dcl-c wup 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';                            
       Dcl-c wlp '0000000000000000000000000000000000000000000000000000';                            
                                                                                                    
     DDirent           Ds                  Based(P_dirent)                                          
     Dd_reserved1                    16a                                                            
     D   D_fileno_gen_id...                                                                         
     D   D_fil                       10u 0                                                          
     D   D_fileno                    10u 0                                                          
     D   D_reclen                    10u 0                                                          
     D   D_reserved3                 10i 0                                                          
     D   D_reserved4                  8a                                                            
     D   D_nlsinfo                   12a                                                            
     D     Nls_ccsid                 10i 0 Overlay(D_nlsinfo:1)                                     
     D     Nls_cntry                  2a   Overlay(D_nlsinfo:5)                                     
     D     Nls_lang                   3a   Overlay(D_nlsinfo:7)                                     
     D     Nls_reserv                 3a   Overlay(D_nlsinfo:10)                                    
     D   Len                         10u 0                                                          
     D   FileName                   640a                                                            
                                                                                                    
     DOccurDatos       Ds                  Occurs(9999)                                             
     DDatosF                       1000a                                                            
     DDsCalculaN       Ds                  Occurs(999)                                              
     D dsConsecutivo                  8a                                                            
     D dsarchivo                     10a                                                            
                                                                                                    
     D                 Ds                                                                           
     DW_ENTERO                 1     10                                                             
     DW_cuenta                 9     15  0                                                          
     DSeparador        Ds                                                                           
     D                                6    Inz('RegAch')                                            
     D                                6    Inz('TipTra')                                            
     D                                6    Inz('Valor')                                             
     D                                6    Inz('Cedula')                                            
     D                                6    Inz('FecTra')                                            
     D                                6    Inz('Camad1')                                            
     D                                6    Inz('Camad2')                                            
                                                                                                    
      ** Definición de Variables de la Estructura por Separadores                                   
     DVecPosNom                     500a   Dim(7) Inz(*Blanks)                                      
     DPosSepara                       3s 0 Dim(7) Inz(999)                                          
     DPosSeparaX                      3s 0 Dim(7) Inz(999)                                          
     DVctSepara                       6    Dim(7) Inz(*Blanks)                                      
     D                                     Overlay(Separador)                                       
                                                                                                    
     ** Definiciòn de Prototipos de Trabajo                                                         
     DProcesarArchivo  Pr                                                                           
     DEstructura       Pr                                                                           
     DSeparadorSql     Pr                                                                           
     DGrabarOccurs     Pr                                                                           
     DLeerOccurs       Pr                                                                           
     DCrearTabla       Pr                                                                           
     DBorrarTabla      Pr                                                                           
     DExtraerCampos    Pr                                                                           
     DGrabarReg        Pr                                                                           
     DDeterminaNroSep  Pr                                                                           
     DElementoMayor    Pr                                                                           
     DSeparadoresFile  Pr                                                                           
     DPrOrden          Pr             9                                                             
     DOrden                           9  0                                                          
                                                                                                    
     DBarradeEstado    Pr                                                                           
     DIdx                             5i 0                                                          
                                                                                                    
     DGenerar          Pr                                                                           
     DParchf           s              1a                                                            
                                                                                                    
      ** Abre Directorio                                                                            
     DOpendir          Pr              *   Extproc('opendir')                                       
     DDirname                          *   Value Options(*String)                                   
                                                                                                    
      ** Lee Directorio                                                                             
     DReaddir          Pr              *   Extproc('readdir')                                       
     DDirname                          *   Value Options(*String)                                   
                                                                                                    
      ** Cierra Directorio                                                                          
     DClosedir         Pr            10i 0 Extproc('closedir')                                      
     DDirname                          *   Value Options(*String)                                   
                                                                                                    
      ** Lee Archivo                                                                                
     DRead             Pr            10i 0 Extproc('read')                                          
     DFildes                         10i 0 Value                                                    
     DBuf                              *   Value                                                    
     DNroBytes                       10u 0 Value                                                    
                                                                                                    
      ** Abre Archivo                                                                               
     DOpen             Pr            10i 0 ExtProc('open')                                          
     DNombreArchivo                    *   Value Options(*String)                                   
     DParametrosOpen                 10i 0 Value                                                    
     DModoOpen                       10u 0 Value Options(*NoPass)                                   
     DCodePage                       10u 0 Value Options(*NoPass)                                   
     DNombreFileEnv    Pr                                                                           
                                                                                                    
      ** Cierra Archivo                                                                             
     DClose            Pr            10i 0 ExtProc('close')                                         
     DFileOpen                       10i 0 Value                                                    
     DPrProcesar600    Pr                                                                           
                                                                                                    
      ** Parametros de Entrada al Programa                                                          
     DFields         e Ds                   ExtName('VOTREA00/VTMASBF')                             
     D                                      prefix(Q)                                               
     D*Main             Pr                  Extpgm('Main')                                          
     DVTMCC23R         Pi                                                                           
     DPcia                            3a                                                            
     DPrut                           64a                                                            
     DPfile                         500a                                                            
     DPcdbay                          2a                                                            
     DPcdesy                          3a                                                            
     DParch                           1a                                                            
     DdsAsb                                 likeds(fields)                                          
     DPres                            1a                                                            
                                                                                                    
     DPgmBan600        Pr                  Extpgm('VOTREP00/VTMCCFER')                              
     DPcia                            3a                                                            
     DPrut                           64a                                                            
     Dxfile                          20a                                                            
     DPcdbay                          2a                                                            
     DPcdesy                          3a                                                            
     DParch                           1a                                                            
     DPres                            1a                                                            
     DPNomFile                       20a                                                            
     DPnomBanco                      40a                                                            
     DPnomEstru                      30a                                                            
                                                                                                    
     DQcmdexc          Pr                  Extpgm('QCMDEXC')                                        
     D                             3000a   Const Options(*Varsize)                                  
     D                               15p 5 Const                                                    
     D                                3a   Const Options(*Nopass)                                   
     DConversion       Pr                  Extpgm('VOTREP00/VTMCSBCR')                              
     DYcia                            3                                                             
     DTced                           30                                                             
     DTcns                            9                                                             
       //------------------------------------------------------------------//                       
       //              P r o g r a m a   P r i n c i p a l                 //                       
       //------------------------------------------------------------------//                       
      /Free                                                                                         
         Clear *All OccurDatos;                                                                     
         Clear RowCount;                                                                            
         Clear Psw;                                                                                 
         Clear TotalRecs;                                                                           
         Clear IdValor;                                                                             
         Clear Xreg;                                                                                
         Clear Rfile;                                                                               
         Rfile = %Trim(Pcia) + %Trim(Pcdbay) + %Trim(Pcdesy);                                       
         PrRecuperaCons();                                                                          
         Rfile = %Trim(Rfile) + %Trim(%Editc(W_cuenta:'Z'));                                        
         Pcdbax = %Int(Pcdbay);                                                                     
         Pcdesx = %Int(Pcdesy);                                                                     
         Estructura();                                                                              
         BorrarTabla();                                                                             
         CrearTabla();                                                                              
         Clear OccurDatos;                                                                          
         If Pcia <> '600';                                                                          
          Generar();                                                                                
         ElseIf Pcia = '600';                                                                       
          If Pcdbax = 5 Or Pcdbax = 6;                                                              
           PNomFile = *Blanks;                                                                      
           PNomFile = 'BP' + %Trim(Rfile);                                                          
           PgmBan600(Pcia:prut:xfile:Pcdbay:Pcdesy:Parch:Pres:PnomFile:                             
                     PnomBanco:PnomEstru);                                                          
           PrProcesar600();                                                                         
          ElseIf Pcdbax <> 5 And Pcdbax <> 6;                                                       
           Generar();                                                                               
          Endif;                                                                                    
         Endif;                                                                                     
         If Parch = 'S';                                                                            
          DeterminaNroSep();                                                                        
         Endif;                                                                                     
         If Psw = *Blanks;                                                                          
          LeerOccurs();                                                                             
         Endif;                                                                                     
         PrBorrarReversasBanco();                                                                   
         Pres = Psw;                                                                                
         Return;                                                                                    
        //*Inlr = *On;                                                                              
      /End-Free                                                                                     
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Genera la Información para el Sub File         //                       
       //------------------------------------------------------------------//                       
     PGenerar          B                                                                            
     DGenerar          Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DFileW            s            500a   Inz(*Blanks)                                             
      /Free                                                                                         
       Monitor;                                                                                     
       // Abro Directorio                                                                           
       FileW = *Blanks;                                                                             
       Ruta = *Blanks;                                                                              
       Ruta = %Trim(Prut);                                                                          
       Pos1 = %Scan(' ':Ruta);                                                                      
       Dir = OpenDir(%Subst(Ruta:1:Pos1-1));                                                        
       // Leo Contenido del Directorio                                                              
        P_dirent = ReadDir(Dir);                                                                    
       // Mientras que Hallan Elementos dentro de la Carpeta                                        
          DoW P_dirent <> *Null;                                                                    
           FileW = %Subst(FileName:1:Len);                                                          
           If %SubSt(FileName:1:1) <>'.' And %Trim(FileW) = %Trim(Pfile);                           
            // Procesar Archivo de Respuestas                                                       
             ProcesarArchivo();                                                                     
             Leave;                                                                                 
           Endif;                                                                                   
            // Leo Siguiente Entrada de Directorio                                                  
            P_dirent = ReadDir(Dir);                                                                
           EndDo;                                                                                   
          // Cierra Directorio                                                                      
          Rp = Closedir(Dir);                                                                       
       On-Error;                                                                                    
        // Cierra Directorio                                                                        
        Rp = Closedir(Dir);                                                                         
       EndMon;                                                                                      
      /End-Free                                                                                     
     PGenerar          E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Lee Archivo de la Ifs                          //                       
       //------------------------------------------------------------------//                       
     PProcesarArchivo  B                                                                            
     DProcesarArchivo  Pi                                                                           
     DNull             s              2a   Inz(X'0020')                                             
     DPos              s              2s 0 Inz(*Zeros)                                              
     DPos2             s              3s 0 Inz(*Zeros)                                              
     DPos4             s              3s 0 Inz(*Zeros)                                              
     DLineMax          s             10i 0 Inz(*Zeros)                                              
                                                                                                    
     DO_Rdonly         c                   1                                                        
     DO_Textdata       c                   16777216                                                 
     DCr               c                   x'0D'                                                    
     DLf               c                   x'25'                                                    
                                                                                                    
      /Free                                                                                         
       cLEAR POS3;                                                                                  
       Clear Pos5;                                                                                  
       Clear W_encabezado;                                                                          
       File = FileName;                                                                             
       Pos = %Scan(%SubSt(Null:1:1):File);                                                          
       If Pos > *Zeros;                                                                             
        File = %SubSt(File:1:Pos-1);                                                                
        // Abro File                                                                                
        FileX = %Subst(Ruta:1:Pos1-1) + '/' + %Trim(%SubSt(File:1:Pos-1));                          
        Pos2 = %Scan(' ':FileX);                                                                    
        Rp = Open(%Subst(FileX:1:Pos2-1):O_Textdata + O_Rdonly:819:0);                              
        // Si se Puede Abrir el Archivo                                                             
        If Rp >= *Zeros;                                                                            
         // Leemos Registros del Archivo                                                            
         LineMax = %Size(DatosR) - 2;                                                               
         Dow (Read(Rp:%Addr(Datos):%Size(Datos)) = %Size(Datos));                                   
             If (Datos <> Cr And Datos <> Lf);                                                      
                Datos = %Xlate(' ':'!':Datos);                                                      
                DatosR = %Trim(DatosR) + Datos;                                                     
             Endif;                                                                                 
             If (Datos = Lf Or %Len(DatosR) = LineMax);                                             
                //w_cdt= dsAsb.QAsbCdt;                                                             
                pos3 = %len(%Trim((dsAsb.QAsbCdt)));                                                
                //w_cen= dsAsb.QAsbcen;                                                             
                pos4 = %len(%Trim(dsAsb.QAsbcen));                                                  
                pos5 = %len(%Trim((dsAsb.QAsbdtf)));                                                
                Datosr = %Xlate('!':' ':Datosr);                                                    
                Pos6 = %len(%Trim(Datosr));                                                         
                If %Len(%Trim(DatosR)) > 1                                                          
                And ((%Subst(DatosR:1:pos3) = %Subst(dsAsb.QAsbCdt:1:pos3)                          
                      And Pos3 > *zeros)                                                            
                Or (%Subst(DatosR:(pos6+1)-pos5:pos5) =                                             
                    %Subst(dsAsb.QAsbdtf:1:pos5) And Pos5 > *Zeros));                               
                   GrabarOccurs();                                                                  
                ElseIf %Len(%Trim(DatosR)) > 1 and dsAsb.QAsbcen <> *Blanks                         
                And %Subst(DatosR:1:pos4) = %Subst(dsAsb.QAsbcen:1:pos4);                           
                   W_encabezado = %Xlate('!':' ':Datosr);                                           
                Endif;                                                                              
                DatosR = *Blanks;                                                                   
             Endif;                                                                                 
         EndDo;                                                                                     
         pos3 = %len(%Trim(dsAsb.QAsbCdt));                                                         
         pos4 = %len(%Trim(dsAsb.QAsbcen));                                                         
         pos5 = %len(%Trim((dsAsb.QAsbdtf)));                                                       
         Datosr = %Xlate('!':' ':Datosr);                                                           
         Pos6 = %len(%Trim(Datosr));                                                                
         If %Len(%Trim(DatosR)) > 1                                                                 
            And ((%Subst(DatosR:1:pos3) = %Subst(dsAsb.QAsbCdt:1:pos3)                              
            And Pos3 > *zeros)                                                                      
            Or (%Subst(DatosR:(pos6+1)-pos5:pos5) =                                                 
                %Subst(dsAsb.QAsbdtf:1:pos5) And                                                    
                  Pos5 > *Zeros));                                                                  
            GrabarOccurs();                                                                         
         ElseIf %Len(%Trim(DatosR)) > 1  and dsAsb.QAsbcen <> *Blanks                               
         And %Subst(DatosR:1:pos4) = %Subst(dsAsb.QAsbcen:1:pos4);                                  
            W_encabezado = %Xlate('!':' ':Datosr);                                                  
         Endif;                                                                                     
         DatosR = *Blanks;                                                                          
        // Cerramos Archivo                                                                         
        Rp=Close(Rp);                                                                               
        Endif;                                                                                      
       Endif;                                                                                       
      /End-Free                                                                                     
     PProcesarArchivo  E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento Guarda Información en la Occurs                    //                       
       //------------------------------------------------------------------//                       
     PGrabarOccurs     B                                                                            
     DGrabarOccurs     Pi                                                                           
      /Free                                                                                         
       RowCount += 1;                                                                               
       If RowCount <= 2;                                                                            
        XReg = %Trim(XReg)+%Trim(Datosr);                                                           
       Endif;                                                                                       
       %Occur(OccurDatos) = RowCount;                                                               
       DatosF = DatosR;                                                                             
      /End-Free                                                                                     
     PGrabarOccurs     E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Crear Tabla de Trabajo en la Qgpl              //                       
       //------------------------------------------------------------------//                       
     PEstructura       B                                                                            
     DEstructura       Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DDeci             s              4a   Inz(*Blanks)                                             
      /Free                                                                                         
       // Archivo por Posiciones                                                                    
       VecPosNom = *Blanks;                                                                         
       If Parch = 'P';                                                                              
       SqlNr2 = *Blanks;                                                                            
       Chain(n) (Pcia:Pcdbax:Pcdesx) Vttbcpf;                                                       
       If %Found(Vttbcpf);                                                                          
        PnomBanco = Bcpban;                                                                         
        PnomEstru = Bcpnom;                                                                         
        ODecima = Bcpnde;                                                                           
        // Tipo de Registro Archivo                                                                 
        If Bcppir <> *Zeros;                                                                        
          SqlNr2 = 'RegAch Char(' + %Trim(%Editc(((Bcppfr-Bcppir)+1)                                
                    :'Z')) + ')';                                                                   
        Endif;                                                                                      
         // Tipo de Transacción                                                                     
        If Bcppit <> *Zeros;                                                                        
          SeparadorSql();                                                                           
          SqlNr2 = %Trim(SqlNr2) + %Trim(Sep) +                                                     
                   ' TipTra Char(' + %Trim(%Editc(((Bcppft-Bcppit)+1)                               
                    :'Z')) + ')';                                                                   
          Sep = *Blanks;                                                                            
        Endif;                                                                                      
         // Valor                                                                                   
        If Bcppiv <> *Zeros;                                                                        
         SeparadorSql();                                                                            
         If Bcpnde = *Zeros;                                                                        
          Deci = '0';                                                                               
         Else;                                                                                      
          Deci = %Trim(%Editc(Bcpnde:'Z'));                                                         
         Endif;                                                                                     
          SqlNr2 = %Trim(SqlNr2) + %Trim(Sep) +                                                     
                   ' Valor Numeric(' + %Trim(%Editc(((Bcppfv-Bcppiv)+1)                             
                    :'Z')) + ', ' + %Trim(Deci) + ')';                                              
          Sep = *Blanks;                                                                            
        Endif;                                                                                      
         // Cédula                                                                                  
        If Bcppic <> *Zeros;                                                                        
          SeparadorSql();                                                                           
          SqlNr2 = %Trim(SqlNr2) + %Trim(Sep) +                                                     
                   ' Cedula Char(' + %Trim(%Editc(((Bcppfc-Bcppic)+1)                               
                    :'Z')) + ')';                                                                   
          Sep = *Blanks;                                                                            
        Endif;                                                                                      
         // Fecha Transacción                                                                       
        If Bcppif <> *Zeros;                                                                        
          SeparadorSql();                                                                           
          SqlNr2 = %Trim(SqlNr2) + %Trim(Sep) +                                                     
                   ' FecTra Char(' + %Trim(%Editc(((Bcppff-Bcppif)+1)                               
                    :'Z')) + ')';                                                                   
          Sep = *Blanks;                                                                            
        Endif;                                                                                      
         // Campo Adicional 1                                                                       
        If Bcppi1 <> *Zeros;                                                                        
          SeparadorSql();                                                                           
          SqlNr2 = %Trim(SqlNr2) + %Trim(Sep) +                                                     
                   ' Camad1 Char(' + %Trim(%Editc(((Bcppf1-Bcppi1)+1)                               
                    :'Z')) + ')';                                                                   
          Sep = *Blanks;                                                                            
        Endif;                                                                                      
         // Campo Adicional 2                                                                       
        If Bcppi2 <> *Zeros;                                                                        
          SeparadorSql();                                                                           
          SqlNr2 = %Trim(SqlNr2) + %Trim(Sep) +                                                     
                   ' Camad2 Char(' + %Trim(%Editc(((Bcppf2-Bcppi2)+1)                               
                    :'Z')) + ')';                                                                   
          Sep = *Blanks;                                                                            
        Endif;                                                                                      
       Endif;                                                                                       
       Endif;                                                                                       
                                                                                                    
       // Archivo por Separadores                                                                   
       If Parch = 'S';                                                                              
       SqlNr2 = *Blanks;                                                                            
       Chain(n) (Pcia:Pcdbax:Pcdesx) Vttbcsf;                                                       
       If %Found(Vttbcsf);                                                                          
        Odecima = Bcsndv;                                                                           
        // Tipo de Registro Archivo                                                                 
        If Bcspor <> *Zeros;                                                                        
          PosSepara(1) = Bcspor;                                                                    
          SqlNr2 = 'RegAch Char(200)';                                                              
          VecPosNom(1) = SqlNr2;                                                                    
        Endif;                                                                                      
         // Tipo de Transacción                                                                     
        If Bcspot <> *Zeros;                                                                        
          PosSepara(2) = Bcspot;                                                                    
          SeparadorSql();                                                                           
          VecPosNom(2) = 'TipTra Char(200)';                                                        
          Sep = *Blanks;                                                                            
        Endif;                                                                                      
         // Valor                                                                                   
        If Bcspov <> *Zeros;                                                                        
         PosSepara(3) = Bcspov;                                                                     
         SeparadorSql();                                                                            
         VecPosNom(3) = 'Valor Char(500)';                                                          
         Sep = *Blanks;                                                                             
        Endif;                                                                                      
         // Cédula                                                                                  
        If Bcspoc <> *Zeros;                                                                        
          PosSepara(4) = Bcspoc;                                                                    
          SeparadorSql();                                                                           
          VecPosNom(4) = 'Cedula Char(30)';                                                         
          Sep = *Blanks;                                                                            
        Endif;                                                                                      
         // Fecha Transacción                                                                       
        If Bcspof <> *Zeros;                                                                        
          PosSepara(5) = Bcspof;                                                                    
          SeparadorSql();                                                                           
          VecPosNom(5) = 'FecTra Char(15)';                                                         
          Sep = *Blanks;                                                                            
        Endif;                                                                                      
         // Campo Adicional 1                                                                       
        If Bcspc1 <> *Zeros;                                                                        
          PosSepara(6) = Bcspc1;                                                                    
          SeparadorSql();                                                                           
          VecPosNom(6) = 'Camad1 Char(200)';                                                        
          Sep = *Blanks;                                                                            
        Endif;                                                                                      
         // Campo Adicional 2                                                                       
        If Bcspc2 <> *Zeros;                                                                        
          PosSepara(7) = Bcspc2;                                                                    
          SeparadorSql();                                                                           
          VecPosNom(7) = 'Camad2 Char(200)';                                                        
          Sep = *Blanks;                                                                            
        Endif;                                                                                      
       Endif;                                                                                       
       Endif;                                                                                       
      /End-Free                                                                                     
     PEstructura       E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Separa Campos del Sql Dinamico                 //                       
       //------------------------------------------------------------------//                       
     PSeparadorSql     B                                                                            
     DSeparadorSql     Pi                                                                           
      /Free                                                                                         
       If %Len(%Trim(SqlNr2)) > *Zeros;                                                             
         Sep = ',';                                                                                 
        Else;                                                                                       
         Sep = *Blanks;                                                                             
        Endif;                                                                                      
      /End-Free                                                                                     
     PSeparadorSql     E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Borra Tabla de Trabajo en la Qgpl              //                       
       //------------------------------------------------------------------//                       
     PBorrarTabla      B                                                                            
     DBorrarTabla      Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DCmd              s           3000a   Inz(*Blanks)                                             
     DWind             s              1a   Inz(*Blanks)                                             
      /Free                                                                                         
       Yfile = *Blanks;                                                                             
       If Parch = 'P';                                                                              
        Yfile ='BP' + %Trim(Rfile);                                                                 
       ElseIf Parch = 'S';                                                                          
        Yfile ='BS' + %Trim(Rfile);                                                                 
       Endif;                                                                                       
       Cmd = *Blanks;                                                                               
       Cmd = 'Select Rbsinp As Wind From Vttrbsf' +                                                 
             ' Where Trim(Rbsarc) = ' + '''' + %Trim(Yfile) + '''' +                                
             ' And Trim(Rbscia) = ' + '''' + %Trim(Pcia) + '''';                                    
        Exec Sql                                                                                    
         Prepare Cur9 From :Cmd;                                                                    
       Exec Sql                                                                                     
        Declare Cursor9 Dynamic Scroll Cursor For Cur9;                                             
       Exec Sql                                                                                     
        Open Cursor9;                                                                               
       Exec Sql                                                                                     
        Fetch Cursor9 Into :Wind;                                                                   
                                                                                                    
       If Sqlcod <> 100 And Sqlcod >= *Zeros;                                                       
        If Wind = 'P';                                                                              
                                                                                                    
         Cmd = *Blanks;                                                                             
         Cmd = 'Drop Table Qgpl/' + %Trim(Yfile);                                                   
                                                                                                    
         Exec Sql                                                                                   
          Prepare Cur From :Cmd;                                                                    
                                                                                                    
         Exec Sql                                                                                   
          Execute Cur;                                                                              
                                                                                                    
         If SqlCod = 100 Or SqlCod < *Zeros;                                                        
          Cmd = *Blanks;                                                                            
          Cmd = 'Delete From Qgpl/' + %Trim(Yfile) +' with nc';                                     
                                                                                                    
          Exec Sql                                                                                  
           Prepare Cur From :Cmd;                                                                   
                                                                                                    
          Exec Sql                                                                                  
           Execute Cur;                                                                             
         Endif;                                                                                     
                                                                                                    
         Cmd = *Blanks;                                                                             
         Cmd = 'Delete From Vttrbsf' + ' Where Rbsarc = ' + '''' +                                  
                %Trim(Yfile) + '''' +                                                               
              ' Trim(Rbscia) = ' + '''' + %Trim(Pcia) + '''' +' with nc';                           
                                                                                                    
         Exec Sql                                                                                   
          Prepare Cur10 From :Cmd;                                                                  
                                                                                                    
         Exec Sql                                                                                   
          Execute Cur10;                                                                            
                                                                                                    
        Endif;                                                                                      
       Else;                                                                                        
        Cmd = *Blanks;                                                                              
        Cmd = 'Delete From Qgpl/' + %Trim(Yfile) +' with nc';                                       
                                                                                                    
        Exec Sql                                                                                    
         Prepare Cur From :Cmd;                                                                     
                                                                                                    
        Exec Sql                                                                                    
         Execute Cur;                                                                               
                                                                                                    
       Endif;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close Cursor9;                                                                              
                                                                                                    
      /End-Free                                                                                     
     PBorrarTabla      E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Crear Tabla de Trabajo en la Qgpl              //                       
       //------------------------------------------------------------------//                       
     PCrearTabla       B                                                                            
     DCrearTabla       Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DSqlStr           s           1000a   Inz(*Blanks)                                             
     DIdx1             s              2s 0 Inz(1)                                                   
     DInd              s              2s 0 Inz(*Zeros)                                              
     DSqlNr5           s            500a   Inz(*Blanks)                                             
     DSqlNr6           s            500a   Inz(*Blanks)                                             
     DNull1            s              2a   Inz(X'0020')                                             
      /Free                                                                                         
       // Archivo por Posiciones                                                                    
       SqlNr1 = *Blanks;                                                                            
       SqlNr5 = *Blanks;                                                                            
       SqlNr6 = *Blanks;                                                                            
       SqlNr6 = ', CompaÑia Char(3), Banco Numeric(3, 0 ), E+                                       
                 struc Numeric(3, 0), Consec Integer Generated Always a+                            
                 s Identity, IndEr Char(1), NfilEn Char(500), -                                     
                 FehCar Char(10), HorCar Char(8)';                                                  
       If Parch = 'P';                                                                              
        SqlNr1 = 'Create Table Qgpl/BP' + %Trim(Rfile) + '(';                                       
        //Arma el Sql a Consultar Dinamicamente                                                     
        SqlStr = %Trim(SqlNr1) +  %Trim(SqlNr2) + %Trim(SqlNr6) + ')';                              
                                                                                                    
        Exec Sql                                                                                    
         Prepare Cur From :SqlStr;                                                                  
                                                                                                    
        Exec Sql                                                                                    
          Execute Cur;                                                                              
       Endif;                                                                                       
                                                                                                    
       // Archivo por Separadores                                                                   
       If Parch = 'S' And %Elem(VecPosNom) > *Zeros;                                                
        SqlNr2 = *Blanks;                                                                           
        For Idx1 = 1 To %Elem(PosSepara);                                                           
          If %Lookup(Idx1:PosSepara) > *Zeros;                                                      
           Ind = %Lookup(Idx1:PosSepara);                                                           
           SqlNr5 = VecPosNom(Ind);                                                                 
           If %Len(%Trim(SqlNr5)) > *Zeros And Idx1 > 1;                                            
            Sep = ',';                                                                              
           Else;                                                                                    
            Sep = *Blanks;                                                                          
           Endif;                                                                                   
           If %Len(%Trim(SqlNr5)) > *Zeros;                                                         
           SqlNr2 = %Trim(SqlNr2) + %Trim(Sep) + ' ' +  %Trim(SqlNr5);                              
           SqlNr5 = *Blanks;                                                                        
          Endif;                                                                                    
         Endif;                                                                                     
        Endfor;                                                                                     
         SqlNr1 = 'Create Table Qgpl/BS' +  %Trim(Rfile) + '(';                                     
         //Arma el Sql a Consultar Dinamicamente                                                    
         SqlStr = %Trim(SqlNr1) +  %Trim(SqlNr2) + %Trim(SqlNr6) + ')';                             
                                                                                                    
         Exec Sql                                                                                   
          Prepare Cur From :SqlStr;                                                                 
                                                                                                    
         Exec Sql                                                                                   
           Execute Cur;                                                                             
       Endif;                                                                                       
      /End-Free                                                                                     
     PCrearTabla       E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Lee la Información de la Occurs                //                       
       //------------------------------------------------------------------//                       
     PLeerOccurs       B                                                                            
     DLeerOccurs       Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DIdx              s              5i 0 Inz(1)                                                   
      /Free                                                                                         
       TotalRecs = RowCount;                                                                        
                                                                                                    
       KKsw = *Off;                                                                                 
       If dsAsb.Qasbcia = '001' And                                                                 
          dsAsb.Qasbcdb = 1 And                                                                     
          dsAsb.Qasbcde = 3;                                                                        
                                                                                                    
       Exec Sql                                                                                     
        Drop Table Qtemp/Banco;                                                                     
                                                                                                    
       If SqlCod = 100 Or SqlCod < *Zeros;                                                          
        Exec Sql                                                                                    
         Delete From Qtemp/Banco;                                                                   
       Endif;                                                                                       
                                                                                                    
       If Parch = 'P';                                                                              
        Exec Sql                                                                                    
         Create Table Qtemp/Banco(                                                                  
          Registro Char(1000) Ccsid 284 Default Null,                                               
          Cedula Char(30) Ccsid 284 Default Null,                                                   
          Valor Decimal(12, 0) Default Null);                                                       
                                                                                                    
        Exec Sql                                                                                    
         Label on Table Qtemp/Banco                                                                 
         Is 'Archivo Temporal Registro con Reversa';                                                
                                                                                                    
        Exec Sql                                                                                    
         Label on Column Qtemp/Banco                                                                
         (Registro Text is 'Registro');                                                             
        Endif;                                                                                      
                                                                                                    
       If Parch = 'S';                                                                              
        Exec Sql                                                                                    
         Create Table Qtemp/Banco(                                                                  
          Registro Char(1000) Ccsid 284 Default Null,                                               
          Cedula Char(30) Ccsid 284 Default Null,                                                   
          Valor Char(30) Ccsid 284 Default Null);                                                   
                                                                                                    
        Exec Sql                                                                                    
         Label on Table Qtemp/Banco                                                                 
         Is 'Archivo Temporal Registro con Reversa';                                                
                                                                                                    
        Exec Sql                                                                                    
         Label on Column Qtemp/Banco                                                                
         (Registro Text is 'Registro');                                                             
        Endif;                                                                                      
       Endif;                                                                                       
                                                                                                    
       For Idx = 1 To RowCount;                                                                     
        %Occur(OccurDatos) = Idx;                                                                   
        VecPos = DatosF;                                                                            
        If %Scan('R06':DatosF) > *Zeros;                                                            
         KKsw = *On;                                                                                
        Endif;                                                                                      
        BarradeEstado(Idx);                                                                         
        ExtraerCampos();                                                                            
       EndFor;                                                                                      
       GrabarReg();                                                                                 
      /End-Free                                                                                     
     PLeerOccurs       E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Extrae Solamente los Campos que Necesitamos    //                       
       //------------------------------------------------------------------//                       
     PExtraerCampos    B                                                                            
     DExtraerCampos    Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DXcedula          s             30a   Inz(*Blanks)                                             
     DSqlNr1           s            500a   Inz(*Blanks)                                             
     DSqlNr6           s            500a   Inz(*Blanks)                                             
     DSqlNr7           s            500a   Inz(*Blanks)                                             
     DSqlNr8           s            500a   Inz(*Blanks)                                             
     DSqlStrF          s           2000a   Inz(*Blanks)                                             
     DPosX             s              4s 0 Inz(*Zeros)                                              
     DPosY             s              4s 0 Inz(*Zeros)                                              
     DIdx2             s              2s 0 Inz(1)                                                   
     DI                s              2s 0 Inz(1)                                                   
     DP                s              2s 0 Inz(*Zeros)                                              
     DVecSepV          s           1000a                                                            
     DStrd             s           1000a                                                            
     DSep1             s              2a   Inz(*Blanks)                                             
     DOsw              s              1a   Inz(*Blanks)                                             
     DWbla             s              1a   Inz(*Blanks)                                             
     DIpos             s              4s 0 Inz(*Zeros)                                              
     DPosd             s              4s 0 Inz(*Zeros)                                              
     DWblans           s              1a   Inz(*Blanks)                                             
     DAValor           s             14a   Inz(*Blanks)                                             
     DBValor           s             14a   Inz(*Blanks)                                             
     DValorVec         s              1a   Dim(100)                                                 
     DValorVecD        s              1a   Dim(100)                                                 
     DIdx0             s              3s 0 Inz(1)                                                   
     DIdx1             s              3s 0 Inz(1)                                                   
     DPoos             s              9s 0 Inz(1)                                                   
                                                                                                    
      ** Definición de Estructuras de Trabajo                                                       
     DCValor           Ds                                                                           
     DEntero                         12a   Inz(*Blanks)                                             
     DDecima                          2a   Inz(*Blanks)                                             
                                                                                                    
     DCValor1          Ds                                                                           
     DEntero1                        14a   Inz(*Blanks)                                             
                                                                                                    
     Dds_operd         DS                                                                           
     Dds_wDeci                 1     11  2 iNZ(*zEROS)                                              
     Dds_Decim                 1     11                                                             
                                                                                                    
      ** Definición de Vectores de Trabajo                                                          
     DVecSep           s              3s 0 Dim(999) Inz(999)                                        
     DYcia             s              3                                                             
     DTced             s             30                                                             
     DTcns             s              9                                                             
     DTcns1            s             30                                                             
                                                                                                    
     DWres             s              2a   Inz(*Blanks)                                             
     DWvalor           s            100a   Inz(*Blanks)                                             
     DVerifica         Pr                  Extpgm('VOTREP00/VTMGOB9R')                              
     DWres                            2a                                                            
     DWvalor                        100a                                                            
                                                                                                    
      /Free                                                                                         
       SqlNr2 = *Blanks;                                                                            
       SqlNr3 = *Blanks;                                                                            
       SqlNr7 = *Blanks;                                                                            
       SqlNr8 = *Blanks;                                                                            
       VecSepV = *Blanks;                                                                           
       Xcedula = *Blanks;                                                                           
                                                                                                    
       SqlNr8 = ', ' + '''' + Pcia + '''' + ', ' + Pcdbay + ', ' + Pcdesy +                         
                ', DEFAULT' + ', ' + '''' + Wbla + '''';                                            
       SqlNr7 = ', CompaÑia, Banco, Estruc, Consec, IndEr, NfilEn'+                                 
                ', FehCar, HorCar';                                                                 
                                                                                                    
       // Arma Archivo por Posiciones                                                               
       If Parch = 'P';                                                                              
        // Tipo de Registro Archivo                                                                 
        If Bcppir <> *Zeros;                                                                        
         SqlNr2 = 'RegAch';                                                                         
         If %Len(%SubSt(VecPos:Bcppir:((Bcppfr-Bcppir)+1))) > *Zeros;                               
          SqlNr3 = '''' + %SubSt(VecPos:Bcppir:((Bcppfr-Bcppir)+1)) + '''';                         
         Else;                                                                                      
          SqlNr3 = '''' + Wblans + '''';                                                            
         Endif;                                                                                     
        Endif;                                                                                      
         // Tipo de Transacción                                                                     
        If Bcppit <> *Zeros;                                                                        
         SeparadorSql();                                                                            
         SqlNr2 = %Trim(SqlNr2) + %Trim(Sep) + ' ' + %Trim('TipTra');                               
         If %Len(%SubSt(VecPos:Bcppit:((Bcppft-Bcppit)+1))) > *Zeros;                               
          SqlNr3 = %Trim(SqlNr3) + %Trim(Sep) + ' ' + '''' +                                        
                   %SubSt(VecPos:Bcppit:((Bcppft-Bcppit)+1)) + '''';                                
         Else;                                                                                      
          SqlNr3 = %Trim(SqlNr3) + %Trim(Sep) + ' ' + '''' + Wblans                                 
                   + '''';                                                                          
         Endif;                                                                                     
        Endif;                                                                                      
         // Valor                                                                                   
        If Bcppiv <> *Zeros;                                                                        
         SeparadorSql();                                                                            
         SqlNr2 = %Trim(SqlNr2) + %Trim(Sep) + ' ' + %Trim(' Valor');                               
         Poos = *Zeros;                                                                             
         Poos = (Bcppfv-Bcppiv)+1;                                                                  
         IdValora = %SubSt(VecPos:Bcppiv:Poos);                                                     
         Wres = *Blanks;                                                                            
         WValor = *Blanks;                                                                          
         WValor = IdValora;                                                                         
         Verifica(Wres:Wvalor);                                                                     
         If Wres = 'Si';                                                                            
            //Recupera valor dependiente de parámetro                                               
            Clear w_numero;                                                                         
            Clear wSql;                                                                             
            wSql = 'Select decimal(wec , 16 , ' + %char(Odecima)        +' )' +                     
                   ' from( select SubString( '+ $c  + %Trim(VecPos) + $c  +                         
                   ' , '+ %char(Bcppiv) + ' , ' + %char(Poos-Odecima) +                             
                   ' )||' + $c  +                                                                   
                   ','+$c  +'|| SubString( '+ $c  + %Trim(VecPos) + $c  +                           
                   ' , '+ %char((Bcppfv-Odecima)+1) + ' , ' + %char(Odecima) +                      
                        ' ) as wec  from SYSIBM.sysdummy1 ) ' +                                     
                          'as a Fetch First 1 Rows Only';                                           
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur0 From :wSql;                                                                    
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor0 Cursor For Cur0;                                                            
                                                                                                    
       Exec Sql                                                                                     
        Open Cursor0;                                                                               
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor0 Into :w_numero;                                                               
                                                                                                    
       Exec Sql                                                                                     
        Close Cursor0;                                                                              
                                                                                                    
            IdValor += w_numero;                                                                    
         Endif;                                                                                     
         If %Len(%SubSt(VecPos:Bcppiv:((Bcppfv-Bcppiv)+1))) > *Zeros;                               
          EvalR AValor = %SubSt(VecPos:Bcppiv:((Bcppfv-Bcppiv)+1));                                 
      /End-Free                                                                                     
     C                   MoveA     *All' '       ValorVec                                           
     C                   MoveA     *All' '       ValorVecD                                          
     C                   MoveA     AValor        ValorVec                                           
      /Free                                                                                         
         For Idx0 = 1 To %Elem(ValorVec);                                                           
          If ValorVec(Idx0) <> '.' And ValorVec(Idx0) <> ',' And                                    
             ValorVec(Idx0) <> ' ';                                                                 
           ValorVecD(Idx1) = ValorVec(Idx0);                                                        
           Idx1 += 1;                                                                               
          Endif;                                                                                    
         Endfor;                                                                                    
         Idx1 = 1;                                                                                  
         AValor = *Blanks;                                                                          
      /End-Free                                                                                     
     C                   MoveA     ValorVecD     Avalor                                             
      /Free                                                                                         
          EvalR AValor = %Trim(AValor);                                                             
          AValor = %Xlate(' ':'0':AValor);                                                          
          If ODecima > *Zeros;                                                                      
           CValor = AValor;                                                                         
           BValor = %Trim(%Editc(%Int(Entero):'Z')) + '.' + %Trim(Decima);                          
          Else;                                                                                     
           CValor1 = AValor;                                                                        
           Monitor;                                                                                 
            BValor = %Trim(%Editc(%Int(Entero1):'Z')) + '.' + '00';                                 
           On-Error;                                                                                
            BValor = %Subst(Entero1:1:14);                                                          
           Endmon;                                                                                  
          Endif;                                                                                    
          SqlNr3 = %Trim(SqlNr3) + %Trim(Sep) + ' ' + %Trim(BValor);                                
         Else;                                                                                      
          SqlNr3 = %Trim(SqlNr3) + %Trim(Sep) + ' ' + '0';                                          
         Endif;                                                                                     
        Endif;                                                                                      
         // Cédula                                                                                  
         If %Len(%Trim(Tcns)) <= *Zeros;                                                            
          Tcns = '000000000';                                                                       
         Endif;                                                                                     
        If Bcppic <> *Zeros;                                                                        
         SeparadorSql();                                                                            
         Xcedula = %SubSt(VecPos:Bcppic:((Bcppfc-Bcppic)+1));                                       
         pcedulaa = %SubSt(VecPos:Bcppic:((Bcppfc-Bcppic)+1));                                      
         PrCedulachar30();                                                                          
         If w_P1ced <> *Blanks;                                                                     
            Xcedula = w_P1ced;                                                                      
         EndIf;                                                                                     
         Conversion(Pcia:Xcedula:Tcns);                                                             
         SqlNr2 = %Trim(SqlNr2) + %Trim(Sep) + ' ' + %Trim(' Cedula');                              
         Osw = *Blanks;                                                                             
         Chain(n) (%Int(Tcns)) Lmllst01;                                                            
         If %Found(Lmllst01);                                                                       
          If MlcstÑ <> *Zeros;                                                                      
           Xcedula = *Blanks;                                                                       
           Xcedula = Tcns;                                                                          
           Osw = 'S';                                                                               
          Endif;                                                                                    
         Endif;                                                                                     
         If Osw = *Blanks;                                                                          
          SqlNr3 = %Trim(SqlNr3) + %Trim(Sep) + ' ' + '''' +                                        
                   %SubSt(VecPos:Bcppic:((Bcppfc-Bcppic)+1)) + '''';                                
         Else;                                                                                      
          Tcns1 = %Trim(Xcedula);                                                                   
          SqlNr3 = %Trim(SqlNr3) + %Trim(Sep) + ' ' + '''' + %Trim(Tcns1)                           
                   + '''';                                                                          
         Endif;                                                                                     
        Endif;                                                                                      
         // Fecha Transacción                                                                       
        If Bcppif <> *Zeros;                                                                        
         SeparadorSql();                                                                            
         SqlNr2 = %Trim(SqlNr2) + %Trim(Sep) + ' ' + %Trim(' FecTra');                              
         If dsAsb.QAsbcen = *Blanks;                                                                
            If %Len(%SubSt(VecPos:Bcppif:((Bcppff-Bcppif)+1))) > *Zeros;                            
             SqlNr3 = %Trim(SqlNr3) + %Trim(Sep) + ' ' + '''' +                                     
                      %SubSt(VecPos:Bcppif:((Bcppff-Bcppif)+1)) + '''';                             
            Else;                                                                                   
             SqlNr3 = %Trim(SqlNr3) + %Trim(Sep) + ' ' + '''' + Wblans                              
                      + '''';                                                                       
            Endif;                                                                                  
            Idfec = %SubSt(VecPos:Bcppif:((Bcppff-Bcppif)+1));                                      
         ELseIf dsAsb.QAsbcen <> *Blanks;                                                           
          If %Len(%Trim(W_encabezado)) > 0;                                                         
            If %Len(%SubSt(W_encabezado:Bcppif:((Bcppff-Bcppif)+1))) > *Zeros;                      
             SqlNr3 = %Trim(SqlNr3) + %Trim(Sep) + ' ' + '''' +                                     
                      %SubSt(W_encabezado:Bcppif:((Bcppff-Bcppif)+1)) + '''';                       
            Else;                                                                                   
             SqlNr3 = %Trim(SqlNr3) + %Trim(Sep) + ' ' + '''' + Wblans                              
                      + '''';                                                                       
            Endif;                                                                                  
            Idfec = %SubSt(W_encabezado:Bcppif:((Bcppff-Bcppif)+1));                                
         EndIf;                                                                                     
        Endif;                                                                                      
        Endif;                                                                                      
         // Campo Adicional 1                                                                       
        If Bcppi1 <> *Zeros;                                                                        
         SeparadorSql();                                                                            
         SqlNr2 = %Trim(SqlNr2) + %Trim(Sep) + ' ' + %Trim(' Camad1');                              
         If %Len(%SubSt(VecPos:Bcppi1:((Bcppf1-Bcppi1)+1))) > *Zeros;                               
          SqlNr3 = %Trim(SqlNr3) + %Trim(Sep) + ' ' + '''' +                                        
                   %SubSt(VecPos:Bcppi1:((Bcppf1-Bcppi1)+1)) + '''';                                
         Else;                                                                                      
          SqlNr3 = %Trim(SqlNr3) + %Trim(Sep) + ' ' + '''' + Wblans                                 
                   + '''';                                                                          
         Endif;                                                                                     
        Endif;                                                                                      
         // Campo Adicional 2                                                                       
        If Bcppi2 <> *Zeros;                                                                        
         SeparadorSql();                                                                            
         SqlNr2 = %Trim(SqlNr2) + %Trim(Sep) + ' ' + %Trim(' Camad2');                              
         If %Len(%SubSt(VecPos:Bcppi2:((Bcppf2-Bcppi2)+1))) > *Zeros;                               
          SqlNr3 = %Trim(SqlNr3) + %Trim(Sep) + ' ' + '''' +                                        
                   %SubSt(VecPos:Bcppi2:((Bcppf2-Bcppi2)+1)) + '''';                                
         Else;                                                                                      
          SqlNr3 = %Trim(SqlNr3) + %Trim(Sep) + ' ' + '''' + Wblans                                 
                   + '''';                                                                          
         Endif;                                                                                     
        Endif;                                                                                      
                                                                                                    
        NombreFileEnv();                                                                            
        SqlStrF = *Blanks;                                                                          
        SqlNr1 = 'Insert Into Qgpl/BP' + %Trim(Rfile) + '(';                                        
        //Arma el Sql a Consultar Dinamicamente                                                     
        SqlStrF = %Trim(SqlNr1) + %Trim(SqlNr2) + %Trim(SqlNr7) + ')'                               
                  + ' Values(' + %Trim(SqlNr3) + %Trim(SqlNr8) +                                    
                ', ' + '''' + %Trim(RbsnarW) + '''' + ', ' +                                        
                '''' + %Trim(RbsfcaW) + '''' + ', ' +                                               
                '''' + %Trim(RbshcaW) + '''' + ') with nc';                                         
                                                                                                    
          Exec Sql                                                                                  
           Prepare Cur From :SqlStrF;                                                               
                                                                                                    
          Exec Sql                                                                                  
            Execute Cur;                                                                            
                                                                                                    
        If Kksw = *On;                                                                              
         KKsw = *Off;                                                                               
         Exec Sql                                                                                   
          Insert Into Qtemp/Banco(Registro, Cedula, Valor)                                          
          Values(:DatosF, :Tcns, :Bvalor);                                                          
        Endif;                                                                                      
                                                                                                    
       Endif;                                                                                       
                                                                                                    
       // Arma Archivo por Separadores                                                              
       If Parch = 'S';                                                                              
        SqlNr2 = *Blanks;                                                                           
       // Determina en que Posicones estan las Comas                                                
        VecSepV = VecPos;                                                                           
        Monitor;                                                                                    
        Dow %Scan(%Trim(Bcssep):VecSepV) > *Zeros;                                                  
         VecSep(i) = %Scan(%Trim(Bcssep):VecSepV);                                                  
         Posd = VecSep(i);                                                                          
         VecSepV = %Replace(' ' :VecSepV:%Scan(%Trim(Bcssep):VecSepV):1);                           
         Strd = VecSepV;                                                                            
         i += 1;                                                                                    
        Enddo;                                                                                      
        Ipos = *Zeros;                                                                              
        If Posd > *Zeros;                                                                           
         Ipos = %Len(%Trim(Strd));                                                                  
         Ipos = Ipos - Posd;                                                                        
         Posd = Posd + Ipos + 2;                                                                    
         VecSep(i) = Posd;                                                                          
        Endif;                                                                                      
        On-Error;                                                                                   
        EndMon;                                                                                     
        PosX = *Zeros;                                                                              
        PosY = 1;                                                                                   
        SqlNr6 = *Blanks;                                                                           
        For Idx2 = 1 To %Elem(PosSepara);                                                           
         If %Lookup(Idx2:PosSepara) > *Zeros;                                                       
          P = %Lookup(Idx2:PosSepara);                                                              
         If PosSepara(P) <> 999;                                                                    
          SqlNr6 = %Trim(SqlNr6) + Sep1 + VctSepara(P);                                             
                                                                                                    
          PosX = Posy;                                                                              
          PosY = VecSep(Idx2) - Posx;                                                               
          If Idx2 > 1;                                                                              
           PosX += 1;                                                                               
           PosY -= 1;                                                                               
          Endif;                                                                                    
                                                                                                    
         // Cédula                                                                                  
         If %Len(%Trim(Tcns)) <= *Zeros;                                                            
          Tcns = '000000000';                                                                       
         Endif;                                                                                     
          If  VctSepara(P) = 'Cedula' And Posy > *Zeros;                                            
           Xcedula = %SubSt(VecPos:Posx:(Posy));                                                    
           Conversion(Pcia:Xcedula:Tcns);                                                           
           Osw = *Blanks;                                                                           
           Chain(n) (%Int(Tcns)) Lmllst01;                                                          
           If %Found(Lmllst01);                                                                     
            If MlcstÑ > *Zeros;                                                                     
             Osw = 'S';                                                                             
            Endif;                                                                                  
           Endif;                                                                                   
           If Osw = *Blanks;                                                                        
            SqlNr3 = %Trim(SqlNr3) + %Trim(Sep1) + ' ' +  ''''+                                     
                     %SubSt(VecPos:Posx:(Posy)) + '''';                                             
           Else;                                                                                    
            SqlNr3 = %Trim(SqlNr3) + %Trim(Sep1) + ' ' +  '''' +                                    
                     %Trim(Tcns) + '''';                                                            
           Endif;                                                                                   
          Endif;                                                                                    
                                                                                                    
          If VctSepara(P) <> 'Cedula' And Posy > *Zeros;                                            
           If VctSepara(P) = 'Valor';                                                               
            IdValora = %SubSt(VecPos:Posx:(Posy));                                                  
            Monitor;                                                                                
             IdValor = IdValor + %Int(IdValora);                                                    
            On-Error;                                                                               
             IdValor = IdValor + 0;                                                                 
            Endmon;                                                                                 
           Endif;                                                                                   
                                                                                                    
           If VctSepara(P) = 'FecTra';                                                              
            Idfec = %SubSt(VecPos:Posx:(Posy));                                                     
           Endif;                                                                                   
          SqlNr3 = %Trim(SqlNr3) + %Trim(Sep1) + ' ' +  ''''+                                       
                   %SubSt(VecPos:Posx:(Posy)) + '''';                                               
          Endif;                                                                                    
          PosY = VecSep(Idx2);                                                                      
                                                                                                    
          If %Len(%Trim(SqlNr6)) > *Zeros;                                                          
           Sep1 = ', ';                                                                             
          Else;                                                                                     
           Sep1 = *Blanks;                                                                          
          Endif;                                                                                    
         Endif;                                                                                     
         Endif;                                                                                     
        Endfor;                                                                                     
        RbsnarW = *Blanks;                                                                          
        RbsfcaW = *Blanks;                                                                          
        RbshcaW = *Blanks;                                                                          
        NombreFileEnv();                                                                            
        SqlNr2 = SqlNr6;                                                                            
        SqlStrF = *Blanks;                                                                          
        SqlNr1 = 'Insert Into Qgpl/BS' + %Trim(Rfile) + '(';                                        
        //Arma el Sql a Consultar Dinamicamente                                                     
        SqlStrF = %Trim(SqlNr1) + %Trim(SqlNr2) + %Trim(SqlNr7) + ')'                               
                   + ' Values(' + %Trim(SqlNr3) + %Trim(SqlNr8) +                                   
                ', ' + '''' + %Trim(RbsnarW) + '''' + ', ' +                                        
                '''' + %Trim(RbsfcaW) + '''' + ', ' +                                               
                '''' + %Trim(RbshcaW) + '''' + ') with nc';                                         
          Exec Sql                                                                                  
           Prepare Cur From :SqlStrF;                                                               
                                                                                                    
          Exec Sql                                                                                  
            Execute Cur;                                                                            
                                                                                                    
        If Kksw = *On;                                                                              
         KKsw = *Off;                                                                               
         Exec Sql                                                                                   
          Insert Into Qtemp/Banco(Registro, Cedula, Valor)                                          
          Values(:DatosF, :Tcns, Trim(Char(:IdValor)));                                             
        Endif;                                                                                      
       Endif;                                                                                       
      /End-Free                                                                                     
     PExtraerCampos    E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Actualiza Registro en el Archivo de Bancos     //                       
       // Subidos                                                          //                       
       //------------------------------------------------------------------//                       
     PGrabarReg        B                                                                            
     DGrabarReg        Pi                                                                           
      ** Definiciòn de Variables de Trabajo                                                         
     DYfile            s             10a   Inz(*Blanks)                                             
      /Free                                                                                         
       Chain (Pcia:Pcdbax:%Trim(Xreg)) Vttrbsf;                                                     
       If %Found(Vttrbsf);                                                                          
        Rbsfar = Idfec;                                                                             
        Rbstre = RowCount;                                                                          
        Rbsvto = IdValor;                                                                           
        If Parch = 'P';                                                                             
         Yfile ='BP' + %Trim(Rfile);                                                                
        ElseIf Parch = 'S';                                                                         
         Yfile ='BS' + %Trim(Rfile);                                                                
        Endif;                                                                                      
        Rbsarc = %Trim(Yfile);                                                                      
        Update Regrbs %Fields(Rbsfar:Rbstre:Rbsvto:Rbsarc);                                         
       Endif;                                                                                       
      /End-Free                                                                                     
     PGrabarReg        E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Genera Barra de Estado                         //                       
       //------------------------------------------------------------------//                       
     PBarradeEstado    B                                                                            
     DBarradeEstado    Pi                                                                           
     DIdx                             5i 0                                                          
      ** Definición de Variables de Trabajo                                                         
     DTimeStart        s               T                                                            
     DTimeNow          s               T                                                            
     DTimeHMS          s               T   TimFmt(*Hms)                                             
     DTotalSecs        s              9s 0                                                          
     DTimeHMSa         s              8                                                             
     DCurrentRec       s              7s 0                                                          
     DPct              s              3s 0                                                          
     DIban             s             20a   Inz(*Blanks)                                             
     DIcban            s              2s 0                                                          
                                                                                                    
     C                   Time                    TimeStart                                          
      /Free                                                                                         
       TotalRecs = RowCount;                                                                        
       CurrentRec = CurrentRec + Idx;                                                               
         pct = CurrentRec * 100 / TotalRecs;                                                        
         //Prgbar = '     ' +                                                                       
         //         %Triml(%EditC(CurrentRec:'3')) +                                                
         //         ' Nro de Registros  ' +                                                         
         //         %Triml(%EditC(TotalRecs:'3')) + ' Lectura.'+                                    
         //        %EditW(Pct:'   %');                                                              
         If Pct > 0;                                                                                
         TimeNow = %Time();                                                                         
      /End-Free                                                                                     
     C     TimeNow       SubDur    TimeStart     TotalSecs:*S                                       
      /Free                                                                                         
           TotalSecs = TotalSecs/pct * (100 - pct);                                                 
      /End-Free                                                                                     
     C*    T'00.00.01'   AddDur    TotalSecs:*S  TimeHMS                                            
     C                   MoveL     TimeHMS       Tmp20            20                                
     C                   Move      'Tiempo     ' Tmp20                                              
     C*                  Move      Tmp20         PrgBar                                             
      /Free                                                                                         
         Endif;                                                                                     
         pct = (CurrentRec * 74) / TotalRecs;                                                       
         If Pct > 0;                                                                                
         //%Subst(Prgbar:pct:1) = x'21';                                                            
         Icban = *Zeros;                                                                            
         Icban = %Int(Pcdbay);                                                                      
         Iban = *Blanks;                                                                            
         Exec Sql                                                                                   
          Select Bncdes Into :Iban                                                                  
          From Vtmbncf                                                                              
          Where Bnccod = :Icban;                                                                    
          If Sqlcod <> 100;                                                                         
           //Kban = Iban;                                                                           
          Endif;                                                                                    
          //Write Rdato6;                                                                           
         Endif;                                                                                     
       //%Subst(Prgbar:74:1) = ' ';                                                                 
      /End-Free                                                                                     
     PBarradeEstado    E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Verifica el que Archivo si Tenga el Numero de  //                       
       // Separadores que son                                              //                       
       //------------------------------------------------------------------//                       
     PDeterminaNroSep  B                                                                            
     DDeterminaNroSep  Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DIdx              s              5i 0 Inz(1)                                                   
     DCmd              s           3000a   Inz(*Blanks)                                             
      /Free                                                                                         
       Chain(n) (Pcia:Pcdbax:Pcdesx) Vttbcsf;                                                       
       If %Found(Vttbcsf);                                                                          
        // Tipo de Registro Archivo                                                                 
        If Bcspor <> *Zeros;                                                                        
          PosSeparaX(1) = Bcspor;                                                                   
        Endif;                                                                                      
         // Tipo de Transacción                                                                     
        If Bcspot <> *Zeros;                                                                        
          PosSeparaX(2) = Bcspot;                                                                   
        Endif;                                                                                      
         // Valor                                                                                   
        If Bcspov <> *Zeros;                                                                        
         PosSeparaX(3) = Bcspov;                                                                    
        Endif;                                                                                      
         // Cédula                                                                                  
        If Bcspoc <> *Zeros;                                                                        
          PosSeparaX(4) = Bcspoc;                                                                   
        Endif;                                                                                      
         // Fecha Transacción                                                                       
        If Bcspof <> *Zeros;                                                                        
          PosSeparaX(5) = Bcspof;                                                                   
        Endif;                                                                                      
         // Campo Adicional 1                                                                       
        If Bcspc1 <> *Zeros;                                                                        
          PosSeparaX(6) = Bcspc1;                                                                   
        Endif;                                                                                      
         // Campo Adicional 2                                                                       
        If Bcspc2 <> *Zeros;                                                                        
          PosSeparaX(7) = Bcspc2;                                                                   
        Endif;                                                                                      
       Endif;                                                                                       
       ElementoMayor();                                                                             
       For Idx = 1 To RowCount;                                                                     
        VecPosY = *Blanks;                                                                          
        %Occur(OccurDatos) = Idx;                                                                   
        VecPosY = DatosF;                                                                           
        SeparadoresFile();                                                                          
        If (NroSepF > (Numsep-1)) Or (NroSepF < (Numsep-1));                                        
         Psw = 'S';                                                                                 
         //Pnror = Idx;                                                                             
         //Pmns = %Subst(VecPosY:1:54);                                                             
         //Exfmt Rdato7;                                                                            
         Cmd = *Blanks;                                                                             
         Cmd = 'Delete From Vttrbsf' + ' Where Trim(Rbsrar) = ' +                                   
               '''' + %Trim(DatosF) + '''' + ' And ' +                                              
              ' Trim(Rbscia) = ' + '''' + %Trim(Pcia) + '''' +                                      
              ' And Rbsinp = ''P''';                                                                
                                                                                                    
         Exec Sql                                                                                   
          Prepare Cur15 From :Cmd;                                                                  
                                                                                                    
         Exec Sql                                                                                   
          Execute Cur15;                                                                            
                                                                                                    
         *Inlr = *On;                                                                               
         Return;                                                                                    
        Endif;                                                                                      
       EndFor;                                                                                      
      /End-Free                                                                                     
     PDeterminaNroSep  E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Determina el Numero de Separadores del Archivo //                       
       //------------------------------------------------------------------//                       
     PElementoMayor    B                                                                            
     DElementoMayor    Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DIdx1             s              5i 0 Inz(1)                                                   
      /Free                                                                                         
        Numsep = *Zeros;                                                                            
        For Idx1 = 1 To %Elem(PosSeparaX);                                                          
         If Numsep <= PosSeparaX(Idx1) And PosSeparaX(Idx1) <> 999;                                 
          Numsep = PosSeparaX(Idx1);                                                                
         Endif;                                                                                     
        Endfor;                                                                                     
      /End-Free                                                                                     
     PElementoMayor    E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Determina el Numero de Separadores que vienen  //                       
       // en el Archivo por Registro                                       //                       
       //------------------------------------------------------------------//                       
     PSeparadoresFile  B                                                                            
     DSeparadoresFile  Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DTextFile         s           1000a   Inz(*Blanks)                                             
     DSepara           s              3a   Inz(*Blanks)                                             
      /Free                                                                                         
       NroSepF = *Zeros;                                                                            
       TextFile = VecPosY;                                                                          
       Separa = '''' + %Trim(Bcssep) + '''';                                                        
       DoW %Scan(%Trim(Bcssep):TextFile) > *Zeros;                                                  
        TextFile = %Replace('':TextFile:%Scan(%Trim(Bcssep):TextFile):1);                           
        NroSepF += 1;                                                                               
       EndDo;                                                                                       
      /End-Free                                                                                     
     PSeparadoresFile  E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Consulta Nombre del Archivo Enviado por el     //                       
       // Banco                                                            //                       
       //------------------------------------------------------------------//                       
     PNombreFileEnv    B                                                                            
     DNombreFileEnv    Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DCmd              s           3000a   Inz(*Blanks)                                             
     DYfile            s             10a   Inz(*Blanks)                                             
     DWind             s              1a   Inz(*Blanks)                                             
      /Free                                                                                         
        If Parch = 'P';                                                                             
         Yfile ='BP' + %Trim(Rfile);                                                                
        ElseIf Parch = 'S';                                                                         
         Yfile ='BS' + %Trim(Rfile);                                                                
        Endif;                                                                                      
        Cmd = *Blanks;                                                                              
        Cmd = 'Select Rbsnar, Rbsfca, Rbshca ' +                                                    
              'From Vttrbsf' +                                                                      
              ' Where Trim(Rbscba) = ' + %Trim(%Editc(Pcdbax:'Z')) +                                
              ' And Rbsarc = '' ''' + ' And ' +                                                     
              ' Trim(Rbscia) = ' + '''' + %Trim(Pcia) + '''' +                                      
              ' And Trim(Rbsinp) = ''P''';                                                          
                                                                                                    
        Exec Sql                                                                                    
         Prepare Cur9aa From :Cmd;                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor9aa Cursor For Cur9aa;                                                        
                                                                                                    
       Exec Sql                                                                                     
        Open Cursor9aa;                                                                             
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor9aa Into :RbsnarW, :RbsfcaW, :RbshcaW;                                          
                                                                                                    
       If Sqlcod = 100 Or Sqlcod < *Zeros;                                                          
        RbsnarW = *Blanks;                                                                          
        RbsfcaW = *Blanks;                                                                          
        RbshcaW = *Blanks;                                                                          
       Endif;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close Cursor9aa;                                                                            
                                                                                                    
      /End-Free                                                                                     
     PNombreFileEnv    E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Trabaja la Estructura de los Bancos de Puerto  //                       
       // Rico                                                             //                       
       //------------------------------------------------------------------//                       
     PPrProcesar600    B                                                                            
     DPrProcesar600    Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DCmd              s           3000a   Inz(*Blanks)                                             
      /Free                                                                                         
        Cmd = 'Select DetalleT From Qtemp/Pagost';                                                  
                                                                                                    
        Exec Sql                                                                                    
         Prepare Cur9j From :Cmd;                                                                   
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor9j Dynamic Scroll Cursor For Cur9j;                                           
                                                                                                    
       Exec Sql                                                                                     
        Open Cursor9j;                                                                              
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor9j Into :DetalleP;                                                              
                                                                                                    
       Dow Sqlcod <> 100 And SqlCod >= *Zeros;                                                      
        Datosr = %Trim(DetalleP);                                                                   
        GrabarOccurs();                                                                             
        Exec Sql                                                                                    
        Fetch Cursor9j Into :DetalleP;                                                              
       Enddo;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close Cursor9j;                                                                             
                                                                                                    
      /End-Free                                                                                     
     PPrProcesar600    E                                                                            
       //------------------------------------------------------------------//                       
       // Procedimiento recupera cedula de la trama para ser verificada   //                        
       //------------------------------------------------------------------//                       
     PPrCedulachar30   B                                                                            
     DPrCedulachar30   Pi            30                                                             
      ** Definición de Variables de Trabajo                                                         
     DCmd              s           3000a   Inz(*Blanks)                                             
     DWvari            s           3000a   Inz(*Blanks)                                             
     D LngFile         s              9  0                                                          
     D posii           s              9  0                                                          
     DTcns             s              9                                                             
     DIorden           s              9  0                                                          
     DIcedula          s             30                                                             
     DW_Orden          s             30                                                             
      /Free                                                                                         
        Clear w_P1ced;                                                                              
        Clear w_P1cedAux;                                                                           
        Clear Posii  ;                                                                              
        Clear LngFile;                                                                              
        Clear Wvari ;                                                                               
        Clear wSql  ;                                                                               
        Clear Yfile;                                                                                
        If Parch = 'P';                                                                             
         Yfile ='BP' + %Trim(Rfile);                                                                
        ElseIf Parch = 'S';                                                                         
         Yfile ='BS' + %Trim(Rfile);                                                                
        Endif;                                                                                      
        Wvari = 'DSPFFD FILE(Qgpl/' + %Trim(Yfile) + ') OUTPUT(*OUTFILE)'                           
                 + ' OUTFILE(QTEMP/FLDTMP)';                                                        
        Clear Cmd;                                                                                  
        Cmd = %Trim(Wvari);                                                                         
        Callp(E) qcmdexc(Cmd:%Size(Cmd));                                                           
                                                                                                    
        //Arma el Sql a Consultar Dinamicamente                                                     
        Clear wSql  ;                                                                               
        Clear LngFile;                                                                              
        wSql   = 'SELECT WHFLDB FROM QTEMP/FLDTMP ' +                                               
                 'WHERE WHFLDI = ' + '''' +                                                         
                 'CEDULA' + '''';                                                                   
        Exec Sql  Prepare CurDn From :wSql  ;                                                       
        Exec Sql Declare CursorDn Dynamic Scroll Cursor For CurDn;                                  
        Exec Sql Open CursorDn;                                                                     
        Exec Sql Fetch CursorDn Into :LngFile;                                                      
        Exec Sql Close CursorDn;                                                                    
                                                                                                    
        Clear Posii;                                                                                
        Pcedulaa = %ScanRpl('.':'':Pcedulaa);                                                       
        Posii = %Len(%Trim(PCedulaa));                                                              
        If dsAsb.Qasbcia = '001' And                                                                
           dsAsb.Qasbcdb = 1 And                                                                    
           dsAsb.Qasbcde = 3;                                                                       
                                                                                                    
         Clear Iorden;                                                                              
         Clear Icedula;                                                                             
         Clear PCedulaaB;                                                                           
         Monitor;                                                                                   
          PCedulaaB = %Int(%Trim(PCedulaa));                                                        
         On-Error;                                                                                  
         EndMon;                                                                                    
         If PCedulaaB > 0;                                                                          
          PCedulaa = %Trim(%Editc(PCedulaaB:'Z'));                                                  
         Endif;                                                                                     
         PCedulaa = %Xlate(wup:wlp:PCedulaa);                                                       
       //   Iorden = %Int(%Trim(%Subst(PCedulaa:(Posii-9)+1:9)));                                   
         Iorden = %Int(%Trim(%Subst(PCedulaa:1:9)));                                                
                                                                                                    
        If Posii > 30 and LngFile > *Zeros;                                                         
           w_P1cedAux = PCedulaa;                                                                   
           Clear W_Orden;                                                                           
           W_Orden = '0' + %Trim(%Editc(Iorden:'Z'));                                               
           w_P1cedAux = %ScanRpl(%Trim(W_orden):' ':w_P1cedAux);                                    
           Monitor;                                                                                 
            w_P1cedAux1= %Trim(%Char(%Int(%Trim(w_P1cedAux))));                                     
           On-Error;                                                                                
           EndMon;                                                                                  
           Clear Tcns;                                                                              
           Conversion(Pcia:w_P1cedAux1:Tcns);                                                       
           If Tcns = *blanks;                                                                       
              Clear w_P1ced;                                                                        
              Clear w_P1cedAux;                                                                     
           Else;                                                                                    
            w_P1cedAux = Tcns;                                                                      
           EndIf;                                                                                   
        EndIf;                                                                                      
                                                                                                    
         Icedula = %Trim(PrOrden(Iorden));                                                          
        Clear w_P1ced;                                                                              
         If Icedula <> *Blanks;                                                                     
          PCedulaa = Icedula;                                                                       
          Posii = %Len(%Trim(PCedulaa));                                                            
          w_P1ced = PCedulaa;                                                                       
         Endif;                                                                                     
        Endif;                                                                                      
                                                                                                    
        If Posii > 30 and LngFile > *Zeros;                                                         
         Monitor;                                                                                   
           w_P1ced= %Subst(PCedulaa:LngFile-29:30);                                                 
         On-Error;                                                                                  
           w_P1ced= '0';                                                                            
         Endmon;                                                                                    
           Clear Tcns;                                                                              
           Conversion(Pcia:w_P1ced:Tcns);                                                           
           If Tcns = *blanks;                                                                       
              Clear w_P1ced;                                                                        
           EndIf;                                                                                   
        EndIf;                                                                                      
        Return w_P1ced;                                                                             
      /End-Free                                                                                     
     PPrCedulachar30   E                                                                            
       //------------------------------------------------------------------//                       
       // Procedimiento recupera cedula de la trama para ser verificada   //                        
       //------------------------------------------------------------------//                       
     PPrRecuperaCons   B                                                                            
     DPrRecuperaCons   Pi                                                                           
     D V_consec        s              1    dim(9999)                                                
     D w_ttlLen        s              2  0                                                          
     D w_caracter      s              3                                                             
      ** Definición de Variables de Trabajo                                                         
      /Free                                                                                         
       Clear *All DsCalculaN;                                                                       
       Clear W_sql;                                                                                 
       W_sql = ' Select Trim(RBSCIA)||Trim(RBSCBA)||Trim(RBSCES) wvalor, '+                         
               ' Rbsarc ' +                                                                         
               ' From Vttrbsf' ;                                                                    
       If Parch = 'P';                                                                              
          W_sql = %Trim(W_sql) + ' Where Trim(Rbsarc) Like '+$C+ 'BP'+                              
                 %Trim(Rfile) +'%'+ '''' ;                                                          
       ElseIf Parch = 'S';                                                                          
          W_sql = %Trim(W_sql) + ' Where Trim(Rbsarc) Like '+$C+ 'BS'+                              
                 %Trim(Rfile) +'%'+ '''' ;                                                          
       EndIf;                                                                                       
       W_sql = %Trim(W_sql) + ' And Trim(Rbscia) = ' +                                              
             '''' + %Trim(Pcia) + '''' +                                                            
             ' And Trim(RBSINP) = ' + '''' +'P'+ ''''+' order by Rbsarc asc';                       
       Exec Sql Prepare C4 From :W_sql ;                                                            
       Exec Sql Declare Cursor4 Scroll Cursor For C4;                                               
       Exec Sql Open Cursor4;                                                                       
       Exec Sql Fetch From Cursor4                                                                  
       For 99 Rows Into :DsCalculaN;                                                                
       Clear W_cuenta;                                                                              
       Clear Wdx1;                                                                                  
       If Sqler3 > *Zeros;                                                                          
          Clear V_consec;                                                                           
          Monitor;                                                                                  
            For dx1  = 1 to 99;                                                                     
                Clear W_filetemp;                                                                   
                %Occur(DsCalculaN)= dx1;                                                            
                If %Trim(dsarchivo) = *Blanks;                                                      
                    Leave;                                                                          
                EndIf;                                                                              
                Clear w_ttlLen;                                                                     
                Clear w_caracter;                                                                   
                w_ttlLen    = %Len(%Trim(%Subst(dsarchivo:1:2)+                                     
                              (%Trim(dsConsecutivo))));                                             
                w_caracter   = %subst(dsarchivo:w_ttlLen+1:                                         
                               %Len(%Trim(dsarchivo))-w_ttlLen);                                    
                Wdx1         = %Int(%Trim(w_caracter));                                             
                V_consec(Wdx1) = '1';                                                               
            EndFor;                                                                                 
            W_cuenta = %Lookup(' ':V_consec);                                                       
          On-Error;                                                                                 
             Clear W_cuenta;                                                                        
          EndMon;                                                                                   
       ELse;                                                                                        
        W_cuenta = 1;                                                                               
       EndIf;                                                                                       
       If W_cuenta = *Zeros;                                                                        
          W_cuenta = dx1;                                                                           
       EndIf;                                                                                       
       Exec Sql Close Cursor4;                                                                      
       If W_cuenta > 99;                                                                            
        W_cuenta = 1;                                                                               
       Endif;                                                                                       
      /End-Free                                                                                     
     PPrRecuperaCons   E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Consulta la Cedula que esta relacionada a la   //                       
       // Orden                                                            //                       
       //------------------------------------------------------------------//                       
     PPrOrden          B                                                                            
     DPrOrden          Pi             9                                                             
     DOrden                           9  0                                                          
      ** Definición de Variables de Trabajo                                                         
     DCmd              s           3000a   Inz(*Blanks)                                             
     DCedula           s              9a   Inz(*Blanks)                                             
     DCedula1          s              9a   Inz(*Blanks)                                             
     DWsw              s               n   Inz(*Off)                                                
      /Free                                                                                         
        Clear Cmd;                                                                                  
        If Pcia <= '002';                                                                           
         Cmd = 'Select OhcstÑ From Votrea' + Pcia + '/Vlorhdr02 ' +                                 
               'Where OhordÑ = ' + %Trim(%Editc(Orden:'Z')) +                                       
               ' For Read Only';                                                                    
        Else;                                                                                       
         Cmd = 'Select OhcstÑ From Opf' + Pcia + '/Porhdr ' +                                       
               'Where OhordÑ = ' + %Trim(%Editc(Orden:'Z')) +                                       
               ' For Read Only';                                                                    
        Endif;                                                                                      
                                                                                                    
        Exec Sql                                                                                    
         Prepare Cur9t From :Cmd;                                                                   
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor9t Dynamic Scroll Cursor For Cur9t;                                           
                                                                                                    
       Exec Sql                                                                                     
        Open Cursor9t;                                                                              
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor9t Into :Cedula;                                                                
                                                                                                    
       Exec Sql                                                                                     
        Close Cursor9t;                                                                             
                                                                                                    
       If %Len(%Trim(Cedula)) > 0;                                                                  
        If %Trim(Cedula) = %Trim(w_P1cedAux);                                                       
         Cedula = w_P1cedAux;                                                                       
         Wsw = *On;                                                                                 
        Endif;                                                                                      
                                                                                                    
        If Wsw = *Off;                                                                              
        Clear Count;                                                                                
        Clear SearchString;                                                                         
        // Valida si la Cedula de la Compradora Tambien esta como                                   
        // Nro de Orden                                                                             
        SearchString = 'Select Count(0) ' +                                                         
                       'From Opf' + PCia + '/Lmllst01 ' +                                           
                       'Where MlcstÑ = ' + %Trim(%Editc(Orden:'Z'));                                
        Count = CountUsingSql(SearchString) ;                                                       
        If Count > 0;                                                                               
         Clear SearchString;                                                                        
         SearchString = 'Select MlCstÑ ' +                                                          
                        'From Opf' + PCia + '/Lmllst01 ' +                                          
                        'Where MlcstÑ = ' + %Trim(%Editc(Orden:'Z'));                               
                                                                                                    
         Exec Sql                                                                                   
          Prepare Cur9tn From :Cmd;                                                                 
                                                                                                    
        Exec Sql                                                                                    
         Declare Cursor9tn Dynamic Scroll Cursor For Cur9tn;                                        
                                                                                                    
        Exec Sql                                                                                    
         Open Cursor9tn;                                                                            
                                                                                                    
        Exec Sql                                                                                    
         Fetch Cursor9tn Into :Cedula1;                                                             
                                                                                                    
        Exec Sql                                                                                    
         Close Cursor9tn;                                                                           
                                                                                                    
         Cedula = %Trim(%Editc(Orden:'Z'));                                                         
         If Cedula <> Cedula1 And Cedula1 <> *Zeros;                                                
          //Cedula = %Trim(Cedula1);                                                                
          Clear Cedula;                                                                             
         Endif;                                                                                     
                                                                                                    
        Endif;                                                                                      
        Endif;                                                                                      
        Endif;                                                                                      
                                                                                                    
       Return Cedula;                                                                               
      /End-Free                                                                                     
     PPrOrden          E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Borra Registros Enviado por el Banco como      //                       
       // Reversados                                                       //                       
       //------------------------------------------------------------------//                       
     PPrBorrarReversasBanco...                                                                      
     P                 B                                                                            
     DPrBorrarReversasBanco...                                                                      
     D                 Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DCmd              s           3000a   Inz(*Blanks)                                             
     DCedula           s             30a   Inz(*Blanks)                                             
     DValor            s             30a   Inz(*Blanks)                                             
     DDiv              s             10I 0                                                          
     DNum              s              9  0 Inz(*Zeros)                                              
     DNumP             s              9  0 Inz(2)                                                   
      /Free                                                                                         
       Clear Cmd;                                                                                   
       Cmd = 'Select Cedula, Valor From Qtemp/Banco' +                                              
             ' For Read Only';                                                                      
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur9r From :Cmd;                                                                    
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor9r Dynamic Scroll Cursor For Cur9r;                                           
                                                                                                    
       Exec Sql                                                                                     
        Open Cursor9r;                                                                              
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor9r Into :Cedula, :Valor;                                                        
                                                                                                    
       Dow SqlCod <> 100 And SqlCod >= *Zeros;                                                      
        Clear Div;                                                                                  
        Num = PrConteo(Cedula:Valor);                                                               
        If %Rem(Num:NumP) <> *Zeros;                                                                
         //Número Impar                                                                             
         Clear Cmd;                                                                                 
         If Parch = 'P';                                                                            
          Yfile ='BP' + %Trim(Rfile);                                                               
          Cmd = 'Delete From ' +                                                                    
                'Qgpl/' + %Trim(Yfile) + ' A ' +                                                    
                'Where Rrn(A) > ' +                                                                 
              '(Select Min(Rrn(B)) From Qgpl/' + %Trim(Yfile) + ' B ' +                             
              'Where A.Cedula = B.Cedula And A.Valor = B.Valor)' +                                  
              ' And A.Cedula = ' + %Trim(Cedula);                                                   
         Else;                                                                                      
          Yfile ='BS' + %Trim(Rfile);                                                               
          Cmd = 'Delete From ' +                                                                    
                'Qgpl/' + %Trim(Yfile) + ' A ' +                                                    
                'Where Rrn(A) > ' +                                                                 
              '(Select Min(Rrn(B)) From Qgpl/' + %Trim(Yfile) + ' B ' +                             
              'Where A.Cedula = B.Cedula And A.Valor = B.Valor)' +                                  
              ' And A.Cedula = ' + %Trim(Cedula);                                                   
         Endif;                                                                                     
         Exec Sql                                                                                   
          Execute Immediate :Cmd;                                                                   
        Else;                                                                                       
         //Número Par                                                                               
         Clear Cmd;                                                                                 
         If Parch = 'P';                                                                            
          Yfile ='BP' + %Trim(Rfile);                                                               
          Cmd = 'Delete From ' +                                                                    
                'Qgpl/' + %Trim(Yfile) + ' ' +                                                      
                'Where Trim(Cedula) = ' + '''' + %Trim(Cedula) + '''' +                             
                ' And Trim(Char(Valor)) = ' + '''' + %Trim(Valor) + '''';                           
         Else;                                                                                      
          Yfile ='BS' + %Trim(Rfile);                                                               
          Cmd = 'Delete From ' +                                                                    
                'Qgpl/' + %Trim(Yfile) + ' ' +                                                      
                'Where Trim(Cedula) = ' + '''' + %Trim(Cedula) + '''' +                             
                ' And Trim(Char(Valor)) = ' + '''' + %Trim(Valor) + '''';                           
         Endif;                                                                                     
         Exec Sql                                                                                   
          Execute Immediate :Cmd;                                                                   
        Endif;                                                                                      
        Exec Sql                                                                                    
         Fetch Cursor9r Into :Cedula, :Valor;                                                       
       Enddo;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close Cursor9r;                                                                             
      /End-Free                                                                                     
     PPrBorrarReversasBanco...                                                                      
     P                 E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Cuenta Cuantos Registros Enviaron de la Compra //                       
       // dora                                                             //                       
       //------------------------------------------------------------------//                       
     PPrConteo         B                                                                            
     DPrConteo         Pi             9  0                                                          
     DCedula                         30a                                                            
     DValor                          30a                                                            
      ** Definición de Variables de Trabajo                                                         
     DCmd              s           3000a   Inz(*Blanks)                                             
     DNumero           s              9  0 Inz(*Zeros)                                              
      /Free                                                                                         
       Clear Cmd;                                                                                   
       If Parch = 'P';                                                                              
        Yfile ='BP' + %Trim(Rfile);                                                                 
        Cmd = 'Select Count(0) ' +                                                                  
              'From Qgpl/' + %Trim(Yfile) + ' ' +                                                   
              'Where Trim(Cedula) = ' + '''' + %Trim(Cedula) + '''' +                               
              ' And Valor = ' + '''' + %Trim(Valor) + '''' +                                        
              ' For Read Only';                                                                     
       ElseIf Parch = 'S';                                                                          
        Yfile ='BS' + %Trim(Rfile);                                                                 
        Cmd = 'Select Count(0) ' +                                                                  
              'From Qgpl/' + %Trim(Yfile) + ' ' +                                                   
              'Where Trim(Cedula) = ' + '''' + %Trim(Cedula) + '''' +                               
              ' And Trim(Valor) = ' + '''' + %Trim(Valor) + '''' +                                  
              ' For Read Only';                                                                     
       Endif;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur9t1 From :Cmd;                                                                   
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor9t1 Dynamic Scroll Cursor For Cur9t1;                                         
                                                                                                    
       Exec Sql                                                                                     
        Open Cursor9t1;                                                                             
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor9t1 Into :Numero;                                                               
                                                                                                    
       Exec Sql                                                                                     
        Close Cursor9t1;                                                                            
                                                                                                    
       Return Numero;                                                                               
      /End-Free                                                                                     
     PPrConteo         E                                                                            
