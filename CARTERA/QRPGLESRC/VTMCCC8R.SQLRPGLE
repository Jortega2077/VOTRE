***************************************************************************                         
     **                                                                   *                         
     ** Programa      : VTMCCC8R                                          *                         
     ** Descripciòn:  : Lee Archivo Enviado por el Banco                  *                         
     ** Proyecto      : Menú de Opciones Opi Paises                       *                         
     ** Autor         : (Jortega)                                         *                         
     ** Fecha Creaciòn: 15 de Enero de 2009                               *                         
     **                                                                   *                         
     **********************************************************************                         
     ** Definición de Directivas de Compilación                                                     
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
                                                                                                    
     ** Definición de Archivos de Trabajo                                                           
     FVttbcpf   If   e           k Disk                                                             
     FVttbcsf   If   e           k Disk                                                             
     FVttrbsf   Uf   e           k Disk                                                             
     FVtmbncf   If   e           k Disk                                                             
     FLmllst01  If   e           k Disk                                                             
                                                                                                    
     ** Definiciòn de Archivo de Pantallas                                                          
     FVtbnci2p  Cf   e             Workstn Include(Rdato1:Rdato6:Rdato7)                            
     F                                                                                              
                                                                                                    
     ** Definiciòn de Variables de Trabajo                                                          
     DDir              s               *                                                            
     DP_Dirent         s               *                                                            
     DFile             s            500a   Inz(*Blanks)                                             
     DFileX            s            500a   Inz(*Blanks)                                             
     DRp               s             10i 0                                                          
     DNumsep           s              4s 0 Inz(*Zeros)                                              
     DNroSepF          s              4s 0 Inz(*Zeros)                                              
     DDatos            s              1a                                                            
     DDatosR           s           1000a   Varying                                                  
     DRuta             s             70a   Inz(*Blanks)                                             
     DPos1             s              3s 0 Inz(*Zeros)                                              
     DRowCount         s              4s 0                                                          
     DSqlNr1           s            500a   Inz(*Blanks)                                             
     DSqlNr2           s            500a   Inz(*Blanks)                                             
     DSqlNr6           s            500a   Inz(*Blanks)                                             
     DSep              s              1a   Inz(*Blanks)                                             
     DVecPos           s           1000a                                                            
     DVecPosY          s           1000a                                                            
     DPcdbax           s              2s 0 Inz(*Zeros)                                              
     DPcdesx           s              3s 0 Inz(*Zeros)                                              
     DXreg             s           1000    Inz(*Zeros)                                              
     DIdValor          s             11s 2 Inz(*Zeros)                                              
     DIdValora         s             50    Inz(*Blanks)                                             
     DIdfec            s             10a   Inz(*Blanks)                                             
     DRfile            s             10a   Inz(*Blanks)                                             
     DSqlNr3           s           2000a   Inz(*Blanks)                                             
     DTotalRecs        s              7s 0                                                          
     DPsw              s              1a   Inz(*Blanks)                                             
     DODecima          s              4s 0 Inz(*Zeros)                                              
     DRbsnarW          s                   Like(Rbsnar) Inz(*Blanks)                                
     DRbsfcaW          s             10a   Inz(*Blanks)                                             
     DRbshcaW          s             10a   Inz(*Blanks)                                             
     DYfile            s             10a   Inz(*Blanks)                                             
     DPNomFile         s             20a   Inz(*Blanks)                                             
     DDetalleP         s           1000a   Inz(*Blanks)                                             
     DPnomBanco        s             40a   Inz(*Blanks)                                             
     DPnomEstru        s             30a   Inz(*Blanks)                                             
     D W_sql           s           3330a   Inz(*Blanks)                                             
     D $C              c                   ''''                                                     
                                                                                                    
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
                                                                                                    
     D                 Ds                                                                           
     DW_Entero                 1     10                                                             
     DW_cuenta                 9     10  0                                                          
     DOccurDatos       Ds                  Occurs(9999)                                             
     DDatosF                       1000a                                                            
                                                                                                    
     DSeparador        Ds                                                                           
     D                                6    Inz('RegAch')                                            
     D                                6    Inz('TipTra')                                            
     D                                6    Inz('Valor')                                             
     D                                6    Inz('Cedula')                                            
     D                                6    Inz('FecTra')                                            
     D                                6    Inz('Camad1')                                            
     D                                6    Inz('Camad2')                                            
                                                                                                    
      ** Definición de Variables de la Estructura por Separadores                                   
     DVecPosNom                     500a   Dim(7)                                                   
     DPosSepara                       3s 0 Dim(7) Inz(999)                                          
     DPosSeparaX                      3s 0 Dim(7) Inz(999)                                          
     DVctSepara                       6    Dim(7)                                                   
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
     DMostrarDatos     Pr                                                                           
     DGrabarReg        Pr                                                                           
     DDeterminaNroSep  Pr                                                                           
     DElementoMayor    Pr                                                                           
     DSeparadoresFile  Pr                                                                           
                                                                                                    
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
     DMain             Pr                  Extpgm('Main')                                           
     DPcia                            3a                                                            
     DPrut                           64a                                                            
     DPfile                          20a                                                            
     DPcdbay                          2a                                                            
     DPcdesy                          3a                                                            
     DParch                           1a                                                            
     DPres                            1a                                                            
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
     DPcia                            3a                                                            
     DPrut                           64a                                                            
     DPfile                          20a                                                            
     DPcdbay                          2a                                                            
     DPcdesy                          3a                                                            
     DParch                           1a                                                            
     DPres                            1a                                                            
                                                                                                    
     DPgmBan600        Pr                  Extpgm('VOTREP00/VTMCCFER')                              
     DPcia                            3a                                                            
     DPrut                           64a                                                            
     DPfile                          20a                                                            
     DPcdbay                          2a                                                            
     DPcdesy                          3a                                                            
     DParch                           1a                                                            
     DPres                            1a                                                            
     DPNomFile                       20a                                                            
     DPnomBanco                      40a                                                            
     DPnomEstru                      30a                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       //              P r o g r a m a   P r i n c i p a l                 //                       
       //------------------------------------------------------------------//                       
      /Free                                                                                         
         Psw = *Blanks;                                                                             
         Xreg = *Blanks;                                                                            
         Rfile = *Blanks;                                                                           
         Rfile = %Trim(Pcia) + %Trim(%char(%int(Pcdbay))) +                                         
                 %Trim(%char(%int(Pcdesy)));                                                        
         Clear W_sql;                                                                               
         W_sql = ' SELECT Digits(Ifnull( (Max(substring(Trim(Rbsarc) , 6 , '+                       
                 ' LENGTH(Trim(Rbsarc))-2) ) + 1 ) , 1) )   wvalor  '+                              
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
               ' And Trim(RBSINP) = ' + '''' +'P'+ '''';                                            
         Exec Sql Prepare C4 From :W_sql ;                                                          
         Exec Sql Declare Cursor4 Scroll Cursor For C4;                                             
         Exec Sql Open Cursor4;                                                                     
         Exec Sql fetch Cursor4 Into :W_Entero;                                                     
         Exec Sql Close Cursor4;                                                                    
         Rfile = %Trim(Rfile) + %Editc(W_cuenta:'X');                                               
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
           PgmBan600(Pcia:Prut:Pfile:Pcdbay:Pcdesy:Parch:Pres:PnomFile:                             
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
          MostrarDatos();                                                                           
         Endif;                                                                                     
         Pres = Psw;                                                                                
        *Inlr = *On;                                                                                
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
     DPos              s              2s 0                                                          
     DPos2             s              3s 0                                                          
     DLineMax          s             10i 0                                                          
                                                                                                    
     DO_Rdonly         c                   1                                                        
     DO_Textdata       c                   16777216                                                 
     DCr               c                   x'0D'                                                    
     DLf               c                   x'25'                                                    
                                                                                                    
      /Free                                                                                         
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
           If %Len(%Trim(DatosR)) > 1;                                                              
            Datosr = %Xlate('!':' ':Datosr);                                                        
            GrabarOccurs();                                                                         
           Endif;                                                                                   
           DatosR = *Blanks;                                                                        
          Endif;                                                                                    
         EndDo;                                                                                     
          If %Len(%Trim(DatosR)) > 1;                                                               
           Datosr = %Xlate('!':' ':Datosr);                                                         
           GrabarOccurs();                                                                          
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
       If RowCount = 1;                                                                             
        XReg = Datosr;                                                                              
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
        Cmd = 'Select Rbsinp As Wind From Votrea00/Vttrbsf' +                                       
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
                                                                                                    
       If Sqlcod <> 100;                                                                            
        If Wind = 'P';                                                                              
                                                                                                    
         Cmd = *Blanks;                                                                             
         Cmd = 'Drop Table Qgpl/' + %Trim(Yfile);                                                   
                                                                                                    
         Exec Sql                                                                                   
          Prepare Cur From :Cmd;                                                                    
                                                                                                    
         Exec Sql                                                                                   
          Execute Cur;                                                                              
                                                                                                    
         If SqlCod = 100 Or SqlCod < *Zeros;                                                        
          Cmd = *Blanks;                                                                            
          Cmd = 'Delete From Qgpl/' + %Trim(Yfile);                                                 
                                                                                                    
          Exec Sql                                                                                  
           Prepare Cur From :Cmd;                                                                   
                                                                                                    
          Exec Sql                                                                                  
           Execute Cur;                                                                             
         Endif;                                                                                     
                                                                                                    
         Cmd = *Blanks;                                                                             
         Cmd = 'Delete From Votrea00/Vttrbsf' + ' Where Rbsarc = ' + '''' +                         
                %Trim(Yfile) + '''' +                                                               
              ' Trim(Rbscia) = ' + '''' + %Trim(Pcia) + '''';                                       
                                                                                                    
         Exec Sql                                                                                   
          Prepare Cur10 From :Cmd;                                                                  
                                                                                                    
         Exec Sql                                                                                   
          Execute Cur10;                                                                            
                                                                                                    
        Endif;                                                                                      
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
                 s Identity, IndEr Char(1), NfilEn Char(15), -                                      
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
       For Idx = 1 To RowCount;                                                                     
        %Occur(OccurDatos) = Idx;                                                                   
        VecPos = DatosF;                                                                            
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
                                                                                                    
      ** Definición de Vectores de Trabajo                                                          
     DVecSep           s              3s 0 Dim(999) Inz(999)                                        
     DYcia             s              3                                                             
     DTced             s             30                                                             
     DTcns             s              9                                                             
     DTcns1            s             30                                                             
     DConversion       Pr                  Extpgm('VOTREP00/VTMCSBCR')                              
     DYcia                            3                                                             
     DTced                           30                                                             
     DTcns                            9                                                             
                                                                                                    
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
           IdValor = IdValor + %Dec(%Int(IdValora):11:2);                                           
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
         If %Len(%SubSt(VecPos:Bcppif:((Bcppff-Bcppif)+1))) > *Zeros;                               
          SqlNr3 = %Trim(SqlNr3) + %Trim(Sep) + ' ' + '''' +                                        
                   %SubSt(VecPos:Bcppif:((Bcppff-Bcppif)+1)) + '''';                                
         Else;                                                                                      
          SqlNr3 = %Trim(SqlNr3) + %Trim(Sep) + ' ' + '''' + Wblans                                 
                   + '''';                                                                          
         Endif;                                                                                     
         Idfec = %SubSt(VecPos:Bcppif:((Bcppff-Bcppif)+1));                                         
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
       Endif;                                                                                       
      /End-Free                                                                                     
     PExtraerCampos    E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Muestra la Información del Archivo             //                       
       //------------------------------------------------------------------//                       
     PMostrarDatos     B                                                                            
     DMostrarDatos     Pi                                                                           
      ** Definiciòn de Variables de Trabajo                                                         
     DSqlNr1           s            500a   Inz(*Blanks)                                             
     DSqlStrF          s           2000a   Inz(*Blanks)                                             
     DWvari            s            200a   Inz(*Blanks)                                             
     DCmd              s           3000a                                                            
     DLen              s             15p 5                                                          
     DNro              s              4s 0 Inz(*Zeros)                                              
      ** Definiciòn de Prototipos de Trabajo                                                        
     DQcmdexc          Pr                  Extpgm('QCMDEXC')                                        
     D                             3000a   Const Options(*Varsize)                                  
     D                               15p 5 Const                                                    
     D                                3a   Const Options(*Nopass)                                   
      /Free                                                                                         
                                                                                                    
       SqlNr1 = *Blanks;                                                                            
       If Parch = 'P';                                                                              
        SqlNr1 = 'Select Count(*) As Nro From Qgpl/BP' + %Trim(Rfile);                              
       ElseIf Parch = 'S';                                                                          
        SqlNr1 = 'Select Count(*) As Nro From Qgpl/BS' + %Trim(Rfile);                              
       Endif;                                                                                       
        //Arma el Sql a Consultar Dinamicamente                                                     
        SqlStrF = %Trim(SqlNr1);                                                                    
                                                                                                    
        Exec Sql                                                                                    
         Prepare Cur From :SqlStrF;                                                                 
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor1 Dynamic Scroll Cursor For Cur;                                              
                                                                                                    
       Exec Sql                                                                                     
        Open Cursor1;                                                                               
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor1 Into :Nro;                                                                    
                                                                                                    
       If Sqlcod <> 100;                                                                            
        If Nro = *Zeros;                                                                            
         *In44 = *On;                                                                               
         Exfmt Rdato1;                                                                              
         *In44 = *Off;                                                                              
         Chain (Pcia:Pcdbax:Xreg) Vttrbsf;                                                          
         If %Found(Vttrbsf);                                                                        
          Delete Regrbs;                                                                            
         Endif;                                                                                     
        Endif;                                                                                      
       Endif;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close Cursor1;                                                                              
                                                                                                    
                                                                                                    
       // Ejecuta Qry                                                                               
       If Parch = 'P';                                                                              
        Wvari = *Blanks;                                                                            
        Wvari = 'Verdes File(Qgpl/BP' + %Trim(Rfile) + ')';                                         
        Cmd = %Trim(Wvari);                                                                         
        Callp(E) Qcmdexc(Cmd:%Size(Cmd));                                                           
       Endif;                                                                                       
       If Parch = 'S';                                                                              
        Wvari = *Blanks;                                                                            
        Wvari = 'Verdes File(Qgpl/BS' + %Trim(Rfile) + ')';                                         
        Cmd = %Trim(Wvari);                                                                         
        Callp(E) Qcmdexc(Cmd:%Size(Cmd));                                                           
       Endif;                                                                                       
      /End-Free                                                                                     
     PMostrarDatos     E                                                                            
                                                                                                    
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
         Prgbar = '     ' +                                                                         
                  %Triml(%EditC(CurrentRec:'3')) +                                                  
                  ' Nro de Registros  ' +                                                           
                  %Triml(%EditC(TotalRecs:'3')) + ' Lectura.'+                                      
                 %EditW(Pct:'   %');                                                                
         If Pct > 0;                                                                                
         TimeNow = %Time();                                                                         
      /End-Free                                                                                     
     C     TimeNow       SubDur    TimeStart     TotalSecs:*S                                       
      /Free                                                                                         
           TotalSecs = TotalSecs/pct * (100 - pct);                                                 
      /End-Free                                                                                     
     C     T'00.00.00'   AddDur    TotalSecs:*S  TimeHMS                                            
     C                   MoveL     TimeHMS       Tmp20            20                                
     C                   Move      'Tiempo     ' Tmp20                                              
     C                   Move      Tmp20         PrgBar                                             
      /Free                                                                                         
         Endif;                                                                                     
         pct = (CurrentRec * 74) / TotalRecs;                                                       
         If Pct > 0;                                                                                
         %Subst(Prgbar:pct:1) = x'21';                                                              
         Icban = *Zeros;                                                                            
         Icban = %Int(Pcdbay);                                                                      
         Iban = *Blanks;                                                                            
         Exec Sql                                                                                   
          Select Bncdes Into :Iban                                                                  
          From Vtmbncf                                                                              
          Where Bnccod = :Icban;                                                                    
          If Sqlcod <> 100;                                                                         
           Kban = Iban;                                                                             
          Endif;                                                                                    
          Write Rdato6;                                                                             
         Endif;                                                                                     
       %Subst(Prgbar:74:1) = ' ';                                                                   
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
         Pnror = Idx;                                                                               
         Pmns = %Subst(VecPosY:1:54);                                                               
         Exfmt Rdato7;                                                                              
         Cmd = *Blanks;                                                                             
         Cmd = 'Delete From Votrea00/Vttrbsf' + ' Where Trim(Rbsrar) = ' +                          
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
              'From Votrea00/Vttrbsf' +                                                             
              ' Where Trim(Rbscba) = ' + %Trim(%Editc(Pcdbax:'Z')) +                                
              ' And Rbsarc = '' ''' + ' And ' +                                                     
              ' Trim(Rbscia) = ' + '''' + %Trim(Pcia) + '''' +                                      
              ' And Trim(Rbsinp) = ''P''' +                                                         
              ' Order by Rrn(Votrea00.Vttrbsf) desc ' +                                             
              ' Fetch first 1 Row Only ';                                                           
                                                                                                    
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
