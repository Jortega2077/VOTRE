***************************************************************************                         
     **                                                                   *                         
     ** Programa      : VTMCCC9R                                          *                         
     ** Descripciòn:  : Revisar Pagos                                     *                         
     ** Proyecto      : Menú de Opciones Opi Paises                       *                         
     ** Autor         : (Jortega)                                         *                         
     ** Fecha Creaciòn: 22 de Enero de 2009                               *                         
     **                                                                   *                         
     **********************************************************************                         
     ** Definición de Directivas de Compilación                                                     
     HOption(*NoDebugIO) Dftactgrp(*No) Bnddir('VTMGOI12R')                                         
                                                                                                    
       /Define CountUsingSql                                                                        
       /Include Votrep00/Qrpglesrc,Vtmgoi14r                                                        
       /Undefine CountUsingSql                                                                      
                                                                                                    
     ** Definiciòn de Archivo de Trabajo                                                            
     FLmllst01  If   e           k Disk                                                             
     FVttbcpf   If   e           k Disk                                                             
     FVtmbncf   If   e           k Disk                                                             
     FParsdow   If   e           k Disk                                                             
     FVttbcsf   If   e           k Disk                                                             
     FVtwpbcf   If   e           k Disk                                                             
     FVtwcabf2  If a e           k Disk                                                             
     FVttrbsf   Uf   e           k Disk                                                             
     FVttrbsl1  Uf   e           k Disk    Rename(Regrbs:Rgrbs)                                     
     F                                     Prefix(a)                                                
                                                                                                    
     ** Definiciòn de Archivo de Pantallas                                                          
     FVtbnci2p  Cf   e             Workstn                                                          
     F                                     Sfile(Rdatos1:Nrr)                                       
                                                                                                    
     ** Definiciòn de Archivo de Informes de Inconsistencias Pagos                                  
     FVtbnci0s  O    e             Printer Oflind(*In55)                                            
                                                                                                    
     ** Definiciòn de Variables de Trabajo                                                          
     DDias             s              8s 0 Inz(*Zeros)                                              
     DDateln0          s               d                                                            
     DDateln1          s               d                                                            
     DOfecha           s             10a   Inz(*Blanks)                                             
                                                                                                    
     DCount            s             10p 0                                                          
     DSearchString     s           1000                                                             
     DKKsw             s              1    Inz(*Blanks)                                             
     Dasw              s              1    Inz(*Blanks)                                             
     DKsw              s              1    Inz(*Blanks)                                             
     DOsw              s              1    Inz(*Blanks)                                             
     DXcia             s              3    Inz(*Blanks)                                             
     DXpais            s             20    Inz(*Blanks)                                             
     DPtr              s               *   Inz(%Addr(*In))                                          
     DSqlStr           s           1000a   Inz(*Blanks)                                             
     DSqlNr1           s            500a   Inz(*Blanks)                                             
     DSqlNrX           s            500a   Inz(*Blanks)                                             
     DCamposv          s             11a                                                            
     DPopc             s              1a   Inz(*Blanks)                                             
     DNpag             s              6s 0 Inz(*Zeros)                                              
     DVpag             s             14s 2 Inz(*Zeros)                                              
     DPosix            s              4s 0 Inz(*Zeros)                                              
     DCamad1X          s            100a                                                            
     DCamad1Y          s            100a                                                            
     DCamad1a          s            100a                                                            
     DCamad2a          s            100a                                                            
     DTexto            s             10a   Inz('TOT. PAG')                                          
     DHcia             s              3    Inz(*Blanks)                                             
     DHcdb             s              2    Inz(*Blanks)                                             
     DHcde             s              3    Inz(*Blanks)                                             
     DOrd              s              9s 0 Inz(*Zeros)                                              
     DCedh             s              9s 0 Inz(*Zeros)                                              
     DOrden            s              9s 0 Inz(*Zeros)                                              
     DCedulaR          s              9s 0                                                          
     DCampo3           s              3a   Inz(*Blanks)                                             
     DCampo4           s              2a   Inz(*Blanks)                                             
     DCampo5           s              3a   Inz(*Blanks)                                             
     DWvari            s            200a   Inz(*Blanks)                                             
     DCmd              s           3000a                                                            
     DLen              s             15p 5                                                          
     DXbanco           s             40a   Inz(*Blanks)                                             
     DNroInco          s              4s 0 Inz(*Zeros)                                              
     DPsw              s              1a   Inz(*Blanks)                                             
     DHsw              s              1a   Inz(*Blanks)                                             
     DPkVlrx           s             11s 2 Inz(*Zeros)                                              
     DKresG            s              2a   Inz('No')                                                
     DOrrn             s              9s 0 Inz(*Zeros)                                              
     DPosii            s              9s 0 Inz(*Zeros)                                              
     DPOValor          s            500a   Inz(*Blanks)                                             
     DLngFile          s              9  0 Inz(0)                                                   
     DPosxx            s              9  0 Inz(*Zeros)                                              
     DPosyy            s              9  0 Inz(*Zeros)                                              
     DPoszz            s              9  0 Inz(*Zeros)                                              
                                                                                                    
     ** Definiciòn de Estructuras de Trabajo                                                        
     DIndic            Ds                  Based(Ptr)                                               
     DAyuda                            N   Overlay(Indic:1)                                         
     DSalir                            N   Overlay(Indic:3)                                         
     DCancelar                         N   Overlay(Indic:12)                                        
     DSflDsp                           N   Overlay(Indic:85)                                        
     DSflDspCtl                        N   Overlay(Indic:80)                                        
     DSflClr                           N   Overlay(Indic:75)                                        
     DSflEnd                           N   Overlay(Indic:40)                                        
                                                                                                    
     D                Sds                                                                           
     DJob                    244    253                                                             
     DUser                   254    263                                                             
     DSig_sys                195    196  0                                                          
     DMes_sys                276    277  0                                                          
     DDia_sys                278    279  0                                                          
     DAÑo_sys                280    281  0                                                          
                                                                                                    
     DMyDtaara         Ds                  Dtaara(Vtdpnc)                                           
     DProceso                  1      1a                                                            
                                                                                                    
     ** Definiciòn de Prototipos de Trabajo                                                         
     DPrPgosWebStore   Pr              n                                                            
     DLlenarSfl        Pr                                                                           
     DBorraSfl         Pr                                                                           
     DPintarSfl        Pr                                                                           
     DActualizar       Pr                                                                           
     DCrearTablaTmp    Pr                                                                           
     DBorrarTablaTmp   Pr                                                                           
     DInsertartmp      Pr                                                                           
     DValidarActua     Pr                                                                           
     DCartera          Pr                                                                           
     DActualRegF       Pr                                                                           
     DPagosDupli       Pr                                                                           
     DBorrarRegSfl     Pr                                                                           
     DMoverFile        Pr                                                                           
     DInformeIncons    Pr                                                                           
     DTitulosInf       Pr                                                                           
     DDetalleInf       Pr                                                                           
     DTotalInf         Pr                                                                           
     DBorrarFileQgpl   Pr                                                                           
     DPfileXX                        10a                                                            
     DBorrarFileQgplTmp...                                                                          
     D                 Pr                                                                           
     DAcepPagosDobles  Pr                                                                           
     DCalSlado         Pr                                                                           
     DBorrarRegF       Pr                                                                           
     DPrDtaara         Pr              n                                                            
     DPrVarlidarIncon...                                                                            
     D                 Pr              n                                                            
                                                                                                    
     DPgmPais          Pr                  Extpgm('VOTREP00/VTMCSBBR')                              
     DXcia                            3                                                             
     DXpais                          20                                                             
                                                                                                    
     DQcmdexc          Pr                  Extpgm('QCMDEXC')                                        
     D                             3000a   Const Options(*Varsize)                                  
     D                               15p 5 Const                                                    
     D                                3a   Const Options(*Nopass)                                   
                                                                                                    
     DMain             Pr                  Extpgm('Main')                                           
     DPcia                            3a                                                            
     DPfile                          10a                                                            
     DPnbnc                          40a                                                            
     DPcbnx                           2a                                                            
     DPdarc                        1000a                                                            
     DPNror                           4s 0                                                          
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
     DPcia                            3a                                                            
     DPfile                          10a                                                            
     DPnbnc                          40a                                                            
     DPcbnx                           2a                                                            
     DPdarc                        1000a                                                            
     DPNror                           4s 0                                                          
                                                                                                    
      ** Definición de Vectores de Trabajo                                                          
     DVecCedula        s             30a   Dim(5000)                                                
     DVecVeces         s              3s 0 Dim(5000)                                                
                                                                                                    
     DYcia             s              3                                                             
     DTced             s             30                                                             
     DTcns             s              9                                                             
     DConversion       Pr                  Extpgm('VOTREP00/VTMCSBCR')                              
     DYcia                            3                                                             
     DTced                           30                                                             
     DTcns                            9                                                             
                                                                                                    
     DBorrarRegFile    Pr                  Extpgm('VOTREP00/VTMCCCZR')                              
     DValorX           s            100a   Inz(*Blanks)                                             
     DPrComentarioValor...                                                                          
     D                 Pr                                                                           
     DValorX                        100a                                                            
                                                                                                    
     DNumeroValidos    Ds                                                                           
     D                                1    Inz('0')                                                 
     D                                1    Inz('1')                                                 
     D                                1    Inz('2')                                                 
     D                                1    Inz('3')                                                 
     D                                1    Inz('4')                                                 
     D                                1    Inz('5')                                                 
     D                                1    Inz('6')                                                 
     D                                1    Inz('7')                                                 
     D                                1    Inz('8')                                                 
     D                                1    Inz('9')                                                 
      ** Definición de Arreglo de Meses                                                             
     DVctNumeros                      1    Dim(10)                                                  
     D                                     Overlay(NumeroValidos)                                   
     DVctNumerosB                     1    Dim(30) Inz(*Blanks)                                     
     DVctNumerosBn                    1    Dim(30) Inz(*Blanks)                                     
                                                                                                    
       //------------------------------------------------------------------//                       
       //              P r o g r a m a   P r i n c i p a l                 //                       
       //------------------------------------------------------------------//                       
      /Free                                                                                         
       BorrarRegFile();                                                                             
       If PrDtaara() = *On;                                                                         
          //Recupera código de banco y estructura                                                   
          Clear Campo4;                                                                             
          Clear Campo5;                                                                             
          Exec Sql                                                                                  
          Select Digits(Rbscba) , Digits(Rbsces) Into :Campo4, :Campo5                              
          From Vttrbsf                                                                              
          Where Rbscia = :Pcia                                                                      
            And Rbscba = Int(:Pcbnx)                                                                
            And Rbsarc = :Pfile                                                                     
            And Rbsinp = 'P';                                                                       
         Camposv = Pnbnc;                                                                           
         Popc = %Subst(Pfile:2:1);                                                                  
         Campo3 = %Subst(Pfile:3:3);                                                                
         //Campo4 = %Subst(Pfile:6:2);                                                              
         //Campo5 = %Subst(Pfile:8:3);                                                              
         PgmPais(Pcia:Xpais);                                                                       
         CrearTablaTmp();                                                                           
         Dow Salir = *Off;                                                                          
          Psw = *Blanks;                                                                            
          Pfecha = %Char(%Date());                                                                  
          PintarSfl();                                                                              
          If Nrr <= *Zeros And Salir = *Off And Cancelar = *Off;                                    
           *In45 = *On;                                                                             
           Exfmt Rdato1;                                                                            
           *In45 = *Off;                                                                            
           Iter;                                                                                    
          Endif;                                                                                    
          If Ayuda = *On;                                                                           
           Exfmt Rdato5;                                                                            
          Endif;                                                                                    
          If Salir = *On;                                                                           
           Leave;                                                                                   
          Endif;                                                                                    
          If *In07 = *On And Nrr > *Zeros                                                           
            And PrDtaara() = *On;                                                                   
           Cartera();                                                                               
           If Psw = *Blanks;                                                                        
            ActualRegF();                                                                           
            PNror = 1;                                                                              
           Else;                                                                                    
            PNror = *Zeros;                                                                         
           Endif;                                                                                   
           MoverFile();                                                                             
           InformeIncons();                                                                         
          Clear Posix;                                                                              
          Endif;                                                                                    
          If Cancelar = *On;                                                                        
           Posix = *Zeros;                                                                          
           Clear Rcontrol1;                                                                         
           Iter;                                                                                    
          Endif;                                                                                    
         BorrarFileQgpl(Pfile);                                                                     
         If KresG = 'No';                                                                           
          PNror = 1;                                                                                
         Endif;                                                                                     
         //BorrarFileQgplTmp();                                                                     
         Enddo;                                                                                     
        Endif;                                                                                      
        *Inlr = *On;                                                                                
      /End-Free                                                                                     
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Llena el Sub Archivo del Sub File              //                       
       //------------------------------------------------------------------//                       
     PLlenarSfl        B                                                                            
     DLlenarSfl        Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DFecTraa          s            100a                                                            
     DCedulaa          s            100a                                                            
     DValora           s            100a                                                            
     DValorD           s            100a                                                            
     DInderr           s              1a                                                            
     DConsecr          s              9s 0                                                          
     DValorn           s             14s 2                                                          
     DWnombre          s             40a   Inz(*Blanks)                                             
     DWtexto           s             40a   Inz(*Blanks)                                             
     DTnum             s              4a   Inz(*Blanks)                                             
     DKnum             s              4s 0 Inz(*Zeros)                                              
     DIdx              s              4s 0 Inz(1)                                                   
     DValorVec         s              1a   Dim(100)                                                 
     DValorVecD        s              1a   Dim(100)                                                 
     DIdx0             s              3s 0 Inz(1)                                                   
     DIdx1             s              3s 0 Inz(1)                                                   
     DNpgo             s              9s 0 Inz(0)                                                   
     DTvalor           s             11  2 Inz(*Zeros)                                              
                                                                                                    
      /Free                                                                                         
       Clear LngFile;                                                                               
       BorraSfl();                                                                                  
       Ksw = *Blanks;                                                                               
       Psw = *Blanks;                                                                               
       SqlStr = *Blanks;                                                                            
       SqlNr1 = *Blanks;                                                                            
       Wtexto = *Blanks;                                                                            
       If Popc = 'P';                                                                               
       Chain(n) (Campo3:%Int(Campo4):%Int(Campo5)) Vttbcpf;                                         
       If %Found(Vttbcpf);                                                                          
        If Bcpcc1 = 'No';                                                                           
         Ksw = 'N';                                                                                 
        Else;                                                                                       
        Wtexto = %Trim(Bcpnc1);                                                                     
        Endif;                                                                                      
       Endif;                                                                                       
       // Arma la Estructura Requerida del Sql                                                      
       SqlNr1 = 'Select FecTra, Cedula, Valor, Inder, Consec' +                                     
                ' From Qgpl/' + %Trim(Pfile) + ' Order By Cedula with nc';                          
       ElseIf Popc = 'S';                                                                           
       Chain(n) (Campo3:%Int(Campo4):%Int(Campo5)) Vttbcsf;                                         
       If %Found(Vttbcsf);                                                                          
        If Bcscc1 = 'No';                                                                           
         Ksw = 'N';                                                                                 
        Else;                                                                                       
         Wtexto = %Trim(Bcsnc1);                                                                    
        Endif;                                                                                      
       Endif;                                                                                       
                                                                                                    
       // Arma la Estructura Requerida del Sql                                                      
       SqlNr1 = 'Select FecTra, Cedula, Valor, Inder, Consec' +                                     
                ' From Qgpl/' + %Trim(Pfile) + ' Order By Cedula with nc';                          
       Endif;                                                                                       
                                                                                                    
                                                                                                    
       //Arma el Sql a Consultar Dinamicamente                                                      
       SqlStr = %Trim(SqlNr1);                                                                      
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur From :SqlStr;                                                                   
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor Dynamic Scroll Cursor For Cur;                                               
                                                                                                    
       Exec Sql                                                                                     
        Open Cursor;                                                                                
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor Into :FecTraa, :Cedulaa, :Valora, :Inderr, :Consecr;                           
                                                                                                    
        Dow Sqlcod <> 100 And Sqlcod >= *Zeros;                                                     
         Evalr P2val = %Trim(Valora);                                                               
         Clear Posii;                                                                               
                                                                                                    
          Wvari = *Blanks;                                                                          
          Wvari = 'DSPFFD FILE(Qgpl/' + %Trim(Pfile) + ') OUTPUT(*OUTFILE)'                         
                   + ' OUTFILE(QTEMP/FLDTMP)';                                                      
         Cmd = %Trim(Wvari);                                                                        
         Callp(E) Qcmdexc(Cmd:%Size(Cmd));                                                          
                                                                                                    
         //Arma el Sql a Consultar Dinamicamente                                                    
         Clear SqlStr;                                                                              
         Clear LngFile;                                                                             
         SqlStr = 'SELECT WHFLDB FROM QTEMP/FLDTMP ' +                                              
                  'WHERE WHFLDI = ' + '''' +                                                        
                  'CEDULA' + '''';                                                                  
                                                                                                    
         Exec Sql                                                                                   
          Prepare CurDn From :SqlStr;                                                               
                                                                                                    
         Exec Sql                                                                                   
          Declare CursorDn Dynamic Scroll Cursor For CurDn;                                         
                                                                                                    
         Exec Sql                                                                                   
          Open CursorDn;                                                                            
                                                                                                    
         Exec Sql                                                                                   
          Fetch CursorDn Into :LngFile;                                                             
                                                                                                    
         Exec Sql                                                                                   
          Close CursorDn;                                                                           
                                                                                                    
         Clear Posii;                                                                               
         Posii = %Len(%Trim(Cedulaa));                                                              
         EvalR Cedulaa = Cedulaa;                                                                   
         P1ced = Cedulaa;                                                                           
         If Posii > 23;                                                                             
            Clear P1ced;                                                                            
            P1ced= %Subst(Cedulaa:LngFile-22:23);                                                   
         EndIf;                                                                                     
                                                                                                    
      *  If Posii >= 20;                                                                            
      *   Cedulaa = %Subst(Cedulaa:((Posii-LngFile)):(Posii- LngFile)+1);                           
      *  Endif;                                                                                     
      *  P1ced = Cedulaa;                                                                           
         P1men = *Blanks;                                                                           
      /End-Free                                                                                     
     C                   If        Pcia = '200'                                                     
     C                   Movea     *Blanks       VctNumerosBn                                       
     C                   Movel     *Blanks       Ivar             30                                
     C                   Z-add     1             Idx1W             2 0                              
     C                   Z-add     1             IdxW              2 0                              
     C                   Z-add     *Zeros        Idx0W             2 0                              
     C                   Movea     P1ced         VctNumerosB                                        
     C                   For       IdxW = 1 to %Elem(VctNumerosB)                                   
     C                   If        %Lookup(VctNumerosB(IdxW):                                       
     C                             VctNumeros)> *Zeros And                                          
     C                             VctNumerosB(IdxW) = *Zeros                                       
     C                   Else                                                                       
     C                   Leave                                                                      
     C                   Endif                                                                      
     C                   Endfor                                                                     
     C                   For       Idx0W = IdxW to %Elem(VctNumerosB)                               
     C                   Eval      VctNumerosBn(Idx1W) = VctNumerosB(Idx0W)                         
     C                   Eval      Idx1W += 1                                                       
     C                   Endfor                                                                     
     C                   Movea     VctNumerosBn  Tced                                               
     C                   Else                                                                       
     C                   Eval      Tced = %Trim(P1ced)                                              
     C                   Endif                                                                      
      /Free                                                                                         
         Tcns = *Blanks;                                                                            
         Conversion(Pcia:Tced:Tcns);                                                                
         Monitor;                                                                                   
          CedulaR = %Int(Tcns);                                                                     
         On-Error;                                                                                  
          CedulaR = *Zeros;                                                                         
         EndMon;                                                                                    
         Exec Sql                                                                                   
         Select OhordÑ Into :Ord                                                                    
         From Lorhdr01                                                                              
         Where OhordÑ = :Orden And OhcstÑ = :CedulaR;                                               
          If SqlCod = 100;                                                                          
           P1ord = *Zeros;                                                                          
           // Arma la Estructura Requerida del Sql                                                  
           SqlNr1 = *Blanks;                                                                        
           SqlNr1 = 'Select Orden ' +                                                               
                    'From Qgpl/Pgsord ' +                                                           
                    'Where Wpais = ' + '''' + %Trim(Pcia) + '''' + ' And ' +                        
                    'Cedula = ' + '''' +  %Trim(Cedulaa) + '''' + ' And ' +                         
                    'Wconsec = ' + %Trim(%editc(Consecr:'Z'));                                      
                                                                                                    
           Exec Sql                                                                                 
            Prepare CurKK From :SqlNr1;                                                             
                                                                                                    
           Exec Sql                                                                                 
            Declare CursorKK Cursor For CurKK;                                                      
                                                                                                    
           Exec Sql                                                                                 
            Open CursorKK;                                                                          
                                                                                                    
           Exec Sql                                                                                 
           Fetch CursorKK Into :P1ord;                                                              
                                                                                                    
           If Sqlcod = 100;                                                                         
            P1ord = *Zeros;                                                                         
           Endif;                                                                                   
                                                                                                    
           Exec Sql                                                                                 
            Close CursorKK;                                                                         
                                                                                                    
          Else;                                                                                     
           P1ord = Orden;                                                                           
          Endif;                                                                                    
         Wnombre = *Blanks;                                                                         
         *In61 = *Off;                                                                              
         If %len(%Trim(Tcns)) > *Zeros;                                                             
          Monitor;                                                                                  
           If %Len(%Trim(%Editc(%Int(Tcns):'Z'))) > *Zeros;                                         
            CedulaR = %Int(Tcns);                                                                   
         Endif;                                                                                     
          On-Error;                                                                                 
          Endmon;                                                                                   
         Endif;                                                                                     
         Chain(n) CedulaR Lmllst01;                                                                 
         If Not %Found(Lmllst01);                                                                   
          If P1ord = *Zeros;                                                                        
           *In98 = *On;                                                                             
           *In99 = *On;                                                                             
           P1men = 'Identificación Invalida.';                                                      
           Psw = 'S';                                                                               
          Endif;                                                                                    
         Else;                                                                                      
          Wnombre = Mlname;                                                                         
          Hsw = *Blanks;                                                                            
          //CalSlado();                                                                             
          If Mldele = 'D';                                                                          
            Wnombre = Mlname;                                                                       
            *In98 = *On;                                                                            
            *In61 = *On;                                                                            
            P1men = 'Identificación Inactiva.';                                                     
            Psw = 'S';                                                                              
          Endif;                                                                                    
         Endif;                                                                                     
         If %Found(Lmllst01);                                                                       
          If FecTraa = *Blanks Or Fectraa = '00000000';                                             
           *In98 = *On;                                                                             
           *In65 = *On;                                                                             
           P1men = 'Fecha Invalida.';                                                               
           Psw = 'S';                                                                               
          Endif;                                                                                    
          If P1men = *Blanks And %Trim(Valora) = '0';                                               
           *In98 = *On;                                                                             
           *In62 = *On;                                                                             
           P1men = 'Valor en Blanco.';                                                              
           Psw = 'S';                                                                               
          Endif;                                                                                    
         Endif;                                                                                     
         If Inderr = 'I';                                                                           
          *In98 = *On;                                                                              
          *In63 = *On;                                                                              
          P1men = 'Pagos Repetidos.';                                                               
          Psw = 'S';                                                                                
         Endif;                                                                                     
         Pop = *Blanks;                                                                             
         P1fec = FecTraa;                                                                           
         PkvlrX = *Zeros;                                                                           
      /End-Free                                                                                     
     C                   MoveA     *All' '       ValorVec                                           
     C                   MoveA     *All' '       ValorVecD                                          
     C                   MoveA     Valora        ValorVec                                           
     C                   Move      *All' '       ValorD                                             
      /Free                                                                                         
         For Idx0 = 1 To %Elem(ValorVec);                                                           
          If ValorVec(Idx0) = ' ';                                                                  
           Leave;                                                                                   
          Endif;                                                                                    
          If ValorVec(Idx0) <> '.' And ValorVec(Idx0) <> ',' And                                    
             ValorVec(Idx0) <> ' ';                                                                 
           ValorVecD(Idx1) = ValorVec(Idx0);                                                        
           Idx1 += 1;                                                                               
          Endif;                                                                                    
         Endfor;                                                                                    
         Idx1 = 1;                                                                                  
         Valora = *Blanks;                                                                          
      /End-Free                                                                                     
     C                   MoveA     ValorVecD     Valora                                             
      /Free                                                                                         
         EvalR Valora = %Trim(Valora);                                                              
         ValorD = Valora;                                                                           
         Valora = %Xlate(' ':'0':Valora);                                                           
         Monitor;                                                                                   
          Pkvlr = %Dec(Valora:11:2);                                                                
         On-Error;                                                                                  
          Pkvlr = *Zeros;                                                                           
         Endmon;                                                                                    
         // Estructura por Separadores                                                              
         If Bcsndv > *Zeros;                                                                        
          Tnum = '1';                                                                               
           For Idx = 1 to Bcsndv;                                                                   
            Tnum = %Trim(Tnum) + '0';                                                               
           EndFor;                                                                                  
          Knum = %Int(%Trim(Tnum));                                                                 
          PkvlrX = (Pkvlr/Knum);                                                                    
          Pkvlr = PkvlrX;                                                                           
         Endif;                                                                                     
         // Estructura por Posiciones                                                               
         //If Bcpnde > *Zeros And (Pcia = '350' Or Pcia = '500' Or                                  
         //   Pcia = '600');                                                                        
         If Bcpnde > *Zeros;                                                                        
          Tnum = '1';                                                                               
           For Idx = 1 to Bcpnde;                                                                   
            Tnum = %Trim(Tnum) + '0';                                                               
           EndFor;                                                                                  
          Knum = %Int(%Trim(Tnum));                                                                 
          Pkvlr = Pkvlr/Knum;                                                                       
         Endif;                                                                                     
         Valorn = (Pkvlr * (-1));                                                                   
         EvalR P1val = %Trim(%Editc(Valorn:'P'));                                                   
         P1nom = Wnombre;                                                                           
         If Ksw = 'N';                                                                              
          Ptexto = %Trim(Texto) + ' ' +  %Trim(P1val);                                              
         Else;                                                                                      
        SqlNrX = *Blanks;                                                                           
        // Campo Adicional 1                                                                        
        SqlNrX = 'Select Camad1' +                                                                  
                 ' From Qgpl/' + %Trim(Pfile) + ' Where Cedula =' + '''' +                          
                 %Trim(P1ced) + '''' + ' And Valor= ' + '''' +                                      
                 %Trim(ValorD) + ''''+' with nc';                                                   
                                                                                                    
         Exec Sql                                                                                   
          Prepare CurX From :SqlNrX;                                                                
                                                                                                    
       Exec Sql                                                                                     
        Declare CursorX Dynamic Scroll Cursor For CurX;                                             
                                                                                                    
       Exec Sql                                                                                     
        Open CursorX;                                                                               
                                                                                                    
       Exec Sql                                                                                     
        Fetch CursorX Into :Camad1x;                                                                
                                                                                                    
        If Sqlcod = 100;                                                                            
         Camad1y = *Blanks;                                                                         
        Endif;                                                                                      
        Dow Sqlcod <> 100 And Sqlcod >= *Zeros;                                                     
         If Camad1y <> Camad1X;                                                                     
          Camad1y = Camad1X;                                                                        
          Leave;                                                                                    
         Endif;                                                                                     
        Exec Sql                                                                                    
         Fetch CursorX Into :Camad1x;                                                               
        EndDo;                                                                                      
                                                                                                    
       Exec Sql                                                                                     
        Close CursorX;                                                                              
        Ptexto = %Trim(Camad1y);                                                                    
        Endif;                                                                                      
         Pinerr = Inderr;                                                                           
         Pccon = Consecr;                                                                           
         Npag += 1;                                                                                 
         Vpag = Vpag + Valorn;                                                                      
         Nrr += 1;                                                                                  
         Posi = Nrr;                                                                                
         If P1men = *Blanks;                                                                        
          Osw = 'S';                                                                                
         Endif;                                                                                     
         Npgo = *Zeros;                                                                             
         If Pcia <= '002';                                                                          
          If %Len(%Trim(P1val)) > *Zeros;                                                           
           Tvalor = %Dec(%Trim(P1val):11:2) * (-1);                                                 
           Exec Sql                                                                                 
            Select Count(0) Into :Npgo                                                              
            From Votrea00/Vttpewf                                                                   
            Where Pewcia = '003' And                                                                
                 ((Trim(Pewco1) = Trim(:Tcns) Or Trim(Pewco1) = Trim(:Tced)) Or                     
         (Trim(Char(Pewoae)) = Trim(:Tcns) Or Trim(Char(Pewoae)) = Trim(:Tced)))                    
            And :Tvalor Between (Pewval-100) And (Pewval+100) And Pewpap = 'P';                     
                                                                                                    
           If Npgo > *Zeros;                                                                        
            *In94 = *On;                                                                            
           Endif;                                                                                   
          Endif;                                                                                    
         Endif;                                                                                     
         Write Rdatos1;                                                                             
         *In98 = *Off;                                                                              
         *In63 = *Off;                                                                              
         *In65 = *Off;                                                                              
         *In99 = *Off;                                                                              
         *In61 = *Off;                                                                              
         *In94 = *Off;                                                                              
       Exec Sql                                                                                     
        Fetch Cursor Into :FecTraa, :Cedulaa, :Valora, :Inderr, :Consecr;                           
        Enddo;                                                                                      
                                                                                                    
       Exec Sql                                                                                     
        Close Cursor;                                                                               
                                                                                                    
        SflEnd = *On;                                                                               
        If Nrr > *Zeros;                                                                            
         SflDsp = *On;                                                                              
        Else;                                                                                       
         SflDsp = *Off;                                                                             
        Endif;                                                                                      
                                                                                                    
        If Nrr > *Zeros;                                                                            
         If Psw = *Blanks;                                                                          
          *In96 = *On;                                                                              
         Else;                                                                                      
          *In97 = *On;                                                                              
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
       Osw = *Blanks;                                                                               
       Posic = 1;                                                                                   
       Npag = *Zeros;                                                                               
       Vpag = *Zeros;                                                                               
       Nrr = *Zeros;                                                                                
       SflClr = *On;                                                                                
       Write Rcontrol1;                                                                             
       SflClr = *Off;                                                                               
      /End-Free                                                                                     
     PBorraSfl         E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Pinta el Sub Archivo de la Pantalla            //                       
       //------------------------------------------------------------------//                       
     PPintarSfl        B                                                                            
     DPintarSfl        Pi                                                                           
      /Free                                                                                         
       // Verifica que Pagos Estan Duplicados en el Archivo Enviado por el //                       
       // Banco //                                                                                  
       PagosDupli();                                                                                
       // Llena Informacion de Pagos Reportados por el Banco //                                     
       LlenarSfl();                                                                                 
       If Posix > *Zeros;                                                                           
        Posic = Posix;                                                                              
       Endif;                                                                                       
       Piban = %Trim(Camposv);                                                                      
       Pnpag = Npag;                                                                                
       Pvpag = Vpag;                                                                                
       Ppais = Xpais;                                                                               
       SflDspCtl = *On;                                                                             
       Write Rteclas1;                                                                              
       Exfmt Rcontrol1;                                                                             
       SflDspCtl = *Off;                                                                            
       *In96 = *Off;                                                                                
       *In97 = *Off;                                                                                
       If Salir = *Off And Cancelar = *Off And Nrr > *Zeros;                                        
        Readc Rdatos1;                                                                              
         Dow Not %Eof();                                                                            
          Select;                                                                                   
           When Pop = '2';                                                                          
            If P1men <> *Blanks;                                                                    
             Actualizar();                                                                          
             Posix = Posi;                                                                          
             Leave;                                                                                 
             Else;                                                                                  
             *In83 = *On;                                                                           
             Exfmt Rdato1;                                                                          
             *In83 = *Off;                                                                          
             Posix = Posi;                                                                          
             Leave;                                                                                 
            Endif;                                                                                  
           When Pop = '4';                                                                          
            If P1men <> *Blanks;                                                                    
             //Posix = 1;                                                                           
             BorrarRegSfl();                                                                        
            Else;                                                                                   
             *In83 = *On;                                                                           
             Exfmt Rdato1;                                                                          
             *In83 = *Off;                                                                          
             //Posix = Posi;                                                                        
             Leave;                                                                                 
            Endif;                                                                                  
          Endsl;                                                                                    
          Pop = *Blanks;                                                                            
          Update Rdatos1;                                                                           
         Readc Rdatos1;                                                                             
        Enddo;                                                                                      
        Endif;                                                                                      
      /End-Free                                                                                     
     PPintarSfl        E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Actualiza Pago Enviado por el Banco            //                       
       //------------------------------------------------------------------//                       
     PActualizar       B                                                                            
     DActualizar       Pi                                                                           
      /Free                                                                                         
       Clear Rdato2;                                                                                
        P2zon = Mlzipc;                                                                             
        P2ced = P1ced;                                                                              
        P2nom = P1nom;                                                                              
        P2fec = P1fec;                                                                              
        P2vlr = P1val;                                                                              
        P2men = P1men;                                                                              
        Pcmt2 = Ptexto;                                                                             
        P2nroo = P1ord;                                                                             
        Pinderx = Pinerr;                                                                           
        Pccons = Pccon;                                                                             
       Dow Cancelar = *Off;                                                                         
                                                                                                    
        If Pinderx = 'I';                                                                           
         *In54 = *On;                                                                               
        Else;                                                                                       
         *In54 = *Off;                                                                              
        Endif;                                                                                      
        // Estructura por Posiciones                                                                
       If Popc = 'P';                                                                               
        SqlNr1 = *Blanks;                                                                           
        // Campo Adicional 1                                                                        
        SqlNr1 = 'Select Camad1' +                                                                  
                 ' From Qgpl/' + %Trim(Pfile) + ' Where Cedula =' + '''' +                          
                 %Trim(P2ced) + ''''+' with nc';                                                    
                                                                                                    
         Exec Sql                                                                                   
          Prepare Cur1 From :SqlNr1;                                                                
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor1 Dynamic Scroll Cursor For Cur1;                                             
                                                                                                    
       Exec Sql                                                                                     
        Open Cursor1;                                                                               
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor1 Into :Camad1a;                                                                
                                                                                                    
        If Sqlcod <> 100;                                                                           
         P2cp1 = Camad1a;                                                                           
        Else;                                                                                       
         P2cp1 = *Blanks;                                                                           
        EndIf;                                                                                      
                                                                                                    
       Exec Sql                                                                                     
        Close Cursor1;                                                                              
                                                                                                    
        // Campo Adicional 2                                                                        
        SqlNr1 = 'Select Camad2' +                                                                  
                 ' From Qgpl/' + %Trim(Pfile) + ' Where Cedula =' + '''' +                          
                 %Trim(P2ced) + ''''+' with nc';                                                    
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur2 From :SqlNr1;                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor2 Dynamic Scroll Cursor For Cur2;                                             
                                                                                                    
       Exec Sql                                                                                     
        Open Cursor2;                                                                               
                                                                                                    
         Exec Sql                                                                                   
         Fetch Cursor2 Into :Camad2a;                                                               
                                                                                                    
        If Sqlcod <> 100;                                                                           
         P2cp2 = Camad2a;                                                                           
        Else;                                                                                       
         P2cp2 = *Blanks;                                                                           
        EndIf;                                                                                      
                                                                                                    
       Exec Sql                                                                                     
        Close Cursor2;                                                                              
        Exfmt Rdato2;                                                                               
       Endif;                                                                                       
                                                                                                    
        // Estructura por Separadores                                                               
       If Popc = 'S';                                                                               
        SqlNr1 = *Blanks;                                                                           
        // Campo Adicional 1                                                                        
        SqlNr1 = 'Select Camad1' +                                                                  
                 ' From Qgpl/' + %Trim(Pfile) + ' Where Cedula =' + '''' +                          
                 %Trim(P2ced) + ''''+' with nc';                                                    
                                                                                                    
         Exec Sql                                                                                   
          Prepare Cur4 From :SqlNr1;                                                                
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor4 Dynamic Scroll Cursor For Cur4;                                             
                                                                                                    
       Exec Sql                                                                                     
        Open Cursor4;                                                                               
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor4 Into :Camad1a;                                                                
                                                                                                    
        If Sqlcod <> 100;                                                                           
         P2cp1 = Camad1a;                                                                           
        Else;                                                                                       
         P2cp1 = *Blanks;                                                                           
        EndIf;                                                                                      
                                                                                                    
       Exec Sql                                                                                     
        Close Cursor4;                                                                              
                                                                                                    
        // Campo Adicional 2                                                                        
        SqlNr1 = 'Select Camad2' +                                                                  
              ' From Qgpl/' + %Trim(Pfile) + ' Where Cedula =' + '''' +                             
               %Trim(P2ced) + ''''+' with nc';                                                      
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur5 From :SqlNr1;                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor5 Dynamic Scroll Cursor For Cur5;                                             
                                                                                                    
       Exec Sql                                                                                     
        Open Cursor5;                                                                               
                                                                                                    
         Exec Sql                                                                                   
         Fetch Cursor5 Into :Camad2a;                                                               
                                                                                                    
        If Sqlcod <> 100;                                                                           
         P2cp2 = Camad2a;                                                                           
        Else;                                                                                       
         P2cp2 = *Blanks;                                                                           
        EndIf;                                                                                      
                                                                                                    
       Exec Sql                                                                                     
        Close Cursor5;                                                                              
       Endif;                                                                                       
       Dow  Cancelar = *Off And *In08 = *Off;                                                       
       Dow P2ced1 = *Zeros And P2nroo = *Zeros And Cancelar = *Off;                                 
        *In64 = *On;                                                                                
        Exfmt Rdato2;                                                                               
        *In64 = *Off;                                                                               
       Enddo;                                                                                       
       If Cancelar = *Off;                                                                          
        If *In66 = *Off;                                                                            
         Exfmt Rdato2;                                                                              
        Endif;                                                                                      
        ValidarActua();                                                                             
       Endif;                                                                                       
       Enddo;                                                                                       
       If *In08 = *On And Cancelar = *Off;                                                          
        *In08 = *Off;                                                                               
        If Ppagd = 1;                                                                               
         AcepPagosDobles();                                                                         
        Endif;                                                                                      
        If Asw = *Blanks;                                                                           
         InsertarTmp();                                                                             
         Cancelar = *On;                                                                            
        Endif;                                                                                      
       Endif;                                                                                       
       Enddo;                                                                                       
       If Cancelar = *On;                                                                           
        Cancelar = *Off;                                                                            
       Endif;                                                                                       
      /End-Free                                                                                     
     PActualizar       E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Crear Tabla de Trabajo en la Qtemp             //                       
       //------------------------------------------------------------------//                       
     PCrearTablaTmp    B                                                                            
     DCrearTablaTmp    Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DSqlStr           s           1000a   Inz(*Blanks)                                             
      /Free                                                                                         
       SqlNr1 = *Blanks;                                                                            
       SqlNr1 = 'Create Table Qgpl/PgsOrd(Cedula Char(200), Or+                                     
                 den Numeric(9), Comentario Char(30), ' +                                           
                 'Wconsec Numeric(9), ' +                                                           
                 'Wpais Char(3), ' +                                                                
                 'Wfile Char(10), ' +                                                               
                 'Wfecha Char(10))';                                                                
       //Arma el Sql a Consultar Dinamicamente                                                      
       SqlStr = %Trim(SqlNr1);                                                                      
                                                                                                    
        Exec Sql                                                                                    
         Prepare Cur From :SqlStr;                                                                  
                                                                                                    
        Exec Sql                                                                                    
          Execute Cur;                                                                              
      /End-Free                                                                                     
     PCrearTablaTmp    E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Crear Tabla de Trabajo en la Qtemp             //                       
       //------------------------------------------------------------------//                       
     PBorrarTablaTmp   B                                                                            
     DBorrarTablaTmp   Pi                                                                           
      /Free                                                                                         
        Exec Sql                                                                                    
         Delete From Qgpl/PgsOrd Where Cedula = Char(:P1ced) And                                    
         Orden = :P1ord And Wpais = :Pcia And Wconsec = :Pccon;                                     
      /End-Free                                                                                     
     PBorrarTablaTmp   E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Crear Tabla de Trabajo en la Qtemp             //                       
       //------------------------------------------------------------------//                       
     PInsertarTmp      B                                                                            
     DInsertarTmp      Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DCedd             s            100a   Inz(*Blanks)                                             
     DSqlNr1           s            500a   Inz(*Blanks)                                             
     DConsecr1         s              9s 0                                                          
     DVar0             s              1a   Inz(*Blanks)                                             
      /Free                                                                                         
       Clear Posxx;                                                                                 
       Clear Posyy;                                                                                 
       Clear Poszz;                                                                                 
                                                                                                    
       Consecr1 = *Zeros;                                                                           
       Posxx =  %Len(%Trim(P2ced));                                                                 
       Posyy = LngFile;                                                                             
       Poszz = (Posyy - Posxx) + 1;                                                                 
       //If P2nroo = *Zeros;                                                                        
       // Arma la Estructura Requerida del Sql                                                      
       If Poszz > 0 and Posxx > 20;                                                                 
        SqlNr1 = 'Select Consec' +                                                                  
                 ' From Qgpl/' + %Trim(Pfile) +                                                     
        ' Where Substr(Cedula, ' + %Trim(%Editc(Poszz:'Z')) + ', ' +                                
          %Trim(%Editc(Posxx:'Z')) + ') = '                                                         
         + '''' + %Trim(P2ced) + '''' +                                                             
                 ' And Inder <> ' + '''' + Var0 + '''' +                                            
                 ' Order By Cedula, Consec Asc';                                                    
       Else;                                                                                        
        SqlNr1 = 'Select Consec' +                                                                  
                 ' From Qgpl/' + %Trim(Pfile) +                                                     
                 ' Where Cedula = ' + '''' + %Trim(P2ced) + '''' +                                  
                 ' And Inder <> ' + '''' + Var0 + '''' +                                            
                 ' Order By Cedula, Consec Asc';                                                    
       Endif;                                                                                       
       SqlNr1 = %trim(SqlNr1)+' with nc';                                                           
                                                                                                    
       //Arma el Sql a Consultar Dinamicamente                                                      
       SqlStr = %Trim(SqlNr1);                                                                      
                                                                                                    
       Exec Sql                                                                                     
        Prepare CurB From :SqlStr;                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Declare CursorB Cursor For CurB;                                                            
                                                                                                    
       Exec Sql                                                                                     
        Open CursorB;                                                                               
                                                                                                    
       Exec Sql                                                                                     
        Fetch CursorB Into :Consecr1;                                                               
                                                                                                    
        If SqlCod <> 100 And SqlCod >= *Zeros;                                                      
         Pccons = Consecr1;                                                                         
        ElseIf SqlCod = 100;                                                                        
         SqlNr1 = 'Select Consec' +                                                                 
                  ' From Qgpl/' + %Trim(Pfile);                                                     
         If Poszz > 0 and Posxx > 20;                                                               
            SqlNr1 = %trim(SqlNr1)+                                                                 
            ' Where Substr(Cedula, ' + %Trim(%Editc(Poszz:'Z')) + ', ' +                            
              %Trim(%Editc(Posxx:'Z')) + ') = '                                                     
             + '''' + %Trim(P2ced) + '''' +                                                         
                      ' Order By Cedula, Consec Asc';                                               
         Else;                                                                                      
            SqlNr1 = %trim(SqlNr1)+                                                                 
                    ' Where Cedula = ' + '''' + %Trim(P2ced) + '''' +                               
                    ' Order By Cedula, Consec Asc';                                                 
         EndIf;                                                                                     
         SqlNr1 = %trim(SqlNr1)+' with nc';                                                         
                                                                                                    
         //Arma el Sql a Consultar Dinamicamente                                                    
          SqlStr = %Trim(SqlNr1);                                                                   
                                                                                                    
         Exec Sql                                                                                   
          Prepare CurB1 From :SqlStr;                                                               
                                                                                                    
       Exec Sql                                                                                     
        Declare CursorB1 Cursor For CurB1;                                                          
                                                                                                    
       Exec Sql                                                                                     
        Open CursorB1;                                                                              
                                                                                                    
        Exec Sql                                                                                    
         Fetch CursorB1 Into :Consecr1;                                                             
                                                                                                    
         Pccons = Consecr1;                                                                         
                                                                                                    
        Exec Sql                                                                                    
         Close CursorB1;                                                                            
                                                                                                    
        Endif;                                                                                      
                                                                                                    
       Exec Sql                                                                                     
        Close CursorB;                                                                              
                                                                                                    
       //Endif;                                                                                     
        SqlNr1 = *Blanks;                                                                           
        Exec Sql                                                                                    
         Update Qgpl/PgsOrd Set Orden = :P2nroo, Comentario =:Pcmt2                                 
         Where Cedula = Char(:P2ced1) And Orden = :P2nroo                                           
         And Wpais = :Pcia;                                                                         
        If Sqlcod = 100;                                                                            
         Exec Sql                                                                                   
         Insert Into Qgpl/PgsOrd(Cedula, Orden, Comentario, Wconsec,                                
                                 Wpais, Wfile, Wfecha)                                              
                     Values(:P2ced1, :P2nroo, :Pcmt2, :Pccons,                                      
                            :Pcia, :Pfile, :Pfecha);                                                
        Endif;                                                                                      
       If P2nroo <> *Zeros Or P2ced1 <> *Zeros;                                                     
         SqlNr1 = 'Update Qgpl/' + %Trim(Pfile) + ' Set Cedula = ' +                                
             '''' + %Char(P2ced1) + '''' ;                                                          
         If Poszz > 0 and Posxx > 20;                                                               
            SqlNr1 = %trim(SqlNr1)+                                                                 
            ' Where Substr(Cedula, ' + %Trim(%Editc(Poszz:'Z')) + ', ' +                            
            %Trim(%Editc(Posxx:'Z')) + ') = '                                                       
            + '''' + %Trim(P2ced) + '''' +                                                          
            ' And ' + 'Consec = ' + %Char(Pccons);                                                  
         Else;                                                                                      
            SqlNr1 = %trim(SqlNr1)+                                                                 
             ' Where Cedula = ' + '''' +                                                            
             %Trim(P2ced) + '''' + ' And ' + 'Consec = ' + %Char(Pccons);                           
         EndIf;                                                                                     
         SqlNr1 = %trim(SqlNr1)+ ' with nc';                                                        
         Exec Sql                                                                                   
          Prepare Cur From :SqlNr1;                                                                 
                                                                                                    
         Exec Sql                                                                                   
           Execute Cur;                                                                             
                                                                                                    
       Endif;                                                                                       
      /End-Free                                                                                     
     PInsertarTmp      E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Crear Tabla de Trabajo en la Qtemp             //                       
       //------------------------------------------------------------------//                       
     PValidarActua     B                                                                            
     DValidarActua     Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DLsw              s              1a   Inz(*Blanks)                                             
                                                                                                    
      /Free                                                                                         
       Dow Lsw  = *Blanks And Cancelar = *Off;                                                      
                                                                                                    
       *In67 = *Off;                                                                                
       Plnom = *Blanks;                                                                             
       If (P2ced1 <> *Zeros And P2ced1 <> 000000000) Or P2nroo <> *Zeros;                           
        If P2ced1 <> *Zeros;                                                                        
         Chain(n) P2ced1 Lmllst01;                                                                  
         If Not %Found(Lmllst01);                                                                   
          *In69 = *On;                                                                              
          Exfmt Rdato2;                                                                             
          *In69 = *Off;                                                                             
         Else;                                                                                      
          If P2ced1 = *Zeros And P2nroo = *Zeros And Cancelar = *Off;                               
           Dow P2ced1 = *Zeros And P2nroo = *Zeros And Cancelar = *Off;                             
           *In64 = *On;                                                                             
           Exfmt Rdato2;                                                                            
           *In64 = *Off;                                                                            
           Enddo;                                                                                   
          Else;                                                                                     
           *In67 = *On;                                                                             
           *In68 = *On;                                                                             
           Plnom = Mlname;                                                                          
           Exfmt Rdato2;                                                                            
           *In68 = *Off;                                                                            
           Lsw = 'S';                                                                               
          Endif;                                                                                    
         Endif;                                                                                     
        Else;                                                                                       
         Dow P2ced1 = *Zeros And Cancelar = *Off;                                                   
         *In64 = *On;                                                                               
         Exfmt Rdato2;                                                                              
         *In64 = *Off;                                                                              
         Enddo;                                                                                     
        Endif;                                                                                      
        //If P2nroo <> *Zeros;                                                                      
         //Exec Sql                                                                                 
         // Select OhordÑ, OhcstÑ Into :Ord, :CedH                                                  
         // From Lorhdr01                                                                           
         // Where OhordÑ = :P2nroo;                                                                 
         // If SqlCod = 100;                                                                        
         //  Asw = 'S';                                                                             
         //  *In68 = *On;                                                                           
         //  Exfmt Rdato2;                                                                          
         //  *In68 = *Off;                                                                          
         // Else;                                                                                   
          // *In67 = *On;                                                                           
          // P2ced1 = Cedh;                                                                         
         //  Plnom = Mlname;                                                                        
         //  Exfmt Rdato2;                                                                          
         //  *In67 = *Off;                                                                          
         //  Lsw = 'S';                                                                             
         //Endif;                                                                                   
        //Endif;                                                                                    
       Endif;                                                                                       
       Enddo;                                                                                       
      /End-Free                                                                                     
     PValidarActua     E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Crea Registros en Cartera                      //                       
       //------------------------------------------------------------------//                       
     PCartera          B                                                                            
     DCartera          Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DTsw              s              1a   Inz(*Blanks)                                             
     DWsig             s              2a   Inz(*Blanks)                                             
     DWano             s              2a   Inz(*Blanks)                                             
     DWaÑo             s              4a   Inz(*Blanks)                                             
     DWmes             s              2a   Inz(*Blanks)                                             
     DWdia             s              2a   Inz(*Blanks)                                             
     DKban             s              3a   Inz(*Blanks)                                             
     DKord             s              9s 0 Inz(*Zeros)                                              
     DKtip             s              2a   Inz(*Blanks)                                             
     DIdx              s              4s 0 Inz(*Zeros)                                              
     DNrrx             s              4s 0 Inz(*Zeros)                                              
     DSqlNr1           s            500a   Inz(*Blanks)                                             
     DComentarioW      s             30a   Inz(*Blanks)                                             
     DKced             s             15s 0 Inz(*Zeros)                                              
                                                                                                    
      /Free                                                                                         
       SqlNr1 = *Blanks;                                                                            
       If Osw <> *Blanks;                                                                           
       Clear Rdato4;                                                                                
       Dow PopcV = *Blanks;                                                                         
       Exfmt Rdato4;                                                                                
       If PopcV = 'S' Or PopcV = 's';                                                               
       Evalr Wsig = %Char(Sig_sys);                                                                 
       Wsig = %Xlate(' ':'0':Wsig);                                                                 
       Evalr Wano = %Char(AÑo_sys);                                                                 
       Wano = %Xlate(' ':'0':Wano);                                                                 
       WaÑo = Wsig + Wano;                                                                          
       Evalr Wmes = %Char(Mes_sys);                                                                 
       Wmes = %Xlate(' ':'0':Wmes);                                                                 
       Evalr Wdia = %Char(Dia_sys);                                                                 
       Wdia = %Xlate(' ':'0':Wdia);                                                                 
       Nrrx = Nrr;                                                                                  
       For Idx = 1 To Nrrx;                                                                         
        Chain Idx Rdatos1;                                                                          
        // Colocar Nuevo Proceso para los Pagos que estan Entrando por el WebStore                  
        // de Compañia 003                                                                          
        If PrPgosWebStore() = *Off;                                                                 
        If P1men = *Blanks;                                                                         
         Tsw = 'S';                                                                                 
         Kban = %Xlate(' ':'0':Campo4);                                                             
      /End-Free                                                                                     
     C                   If        Pcia = '200'                                                     
     C                   Movea     *Blanks       VctNumerosBn                                       
     C                   Movel     *Blanks       Ivar             30                                
     C                   Z-add     1             Idx1W             2 0                              
     C                   Z-add     1             IdxW              2 0                              
     C                   Z-add     *Zeros        Idx0W             2 0                              
     C                   Movea     P1ced         VctNumerosB                                        
     C                   For       IdxW = 1 to %Elem(VctNumerosB)                                   
     C                   If        %Lookup(VctNumerosB(IdxW):                                       
     C                             VctNumeros)> *Zeros And                                          
     C                             VctNumerosB(IdxW) = *Zeros                                       
     C                   Else                                                                       
     C                   Leave                                                                      
     C                   Endif                                                                      
     C                   Endfor                                                                     
     C                   For       Idx0W = IdxW to %Elem(VctNumerosB)                               
     C                   Eval      VctNumerosBn(Idx1W) = VctNumerosB(Idx0W)                         
     C                   Eval      Idx1W += 1                                                       
     C                   Endfor                                                                     
     C                   Movea     VctNumerosBn  Tced                                               
     C                   Else                                                                       
     C                   Eval      Tced = %Trim(P1ced)                                              
     C                   Endif                                                                      
      /Free                                                                                         
         Tcns = *Blanks;                                                                            
         Conversion(Pcia:Tced:Tcns);                                                                
         Monitor;                                                                                   
          Kced = %Int(Tcns);                                                                        
         On-Error;                                                                                  
          Kced = *Zeros;                                                                            
         EndMon;                                                                                    
         Kord = P1ord;                                                                              
         Ktip = 'P';                                                                                
         Clear *Nokey Rwcab2;                                                                       
         ComentarioW = *Blanks;                                                                     
         Chain (Kban:Kced:Kord:Ktip) Vtwcabf2;                                                      
          If Not %Found(Vtwcabf2);                                                                  
           If Pcia <> '200';                                                                        
            Monitor;                                                                                
             Wcbced = %Int(%Trim(P1ced));                                                           
            On-Error;                                                                               
             Kced = *Zeros;                                                                         
            EndMon;                                                                                 
           Else;                                                                                    
            Monitor;                                                                                
             Wcbced = Kced;                                                                         
            On-Error;                                                                               
             Kced = *Zeros;                                                                         
            EndMon;                                                                                 
           Endif;                                                                                   
           Wcbtip = 'P';                                                                            
           Wcbval = Pkvlr * (-1);                                                                   
          Exec Sql                                                                                  
           Select Orden, Comentario Into :P1ord, :ComentarioW                                       
           From Qgpl/PgsOrd                                                                         
           Where Trim(Cedula) = Trim(:P1ced) And Wconsec = :Pccon                                   
                 And Wpais = :Pcia;                                                                 
           If Sqlcod = 100;                                                                         
            P1ord = *Zeros;                                                                         
            ComentarioW = *Blanks;                                                                  
           Endif;                                                                                   
           ValorX = %Trim(P2val);                                                                   
           PrComentarioValor(ValorX);                                                               
           If Pcia = '600';                                                                         
            ComentarioW = %Trim(Camad1x);                                                           
           Endif;                                                                                   
           Wcbord = P1ord;                                                                          
           Xbanco = *Blanks;                                                                        
           Chain(n) (%Int(Pcbnx)) Vtmbncf;                                                          
           If %Found(Vtmbncf);                                                                      
            Wcbbco = %Trim(Bnctnd);                                                                 
            Xbanco = %Trim(Bncdes);                                                                 
           Endif;                                                                                   
           If ComentarioW = *Blanks;                                                                
            Wcbcmn = Ptexto;                                                                        
           Else;                                                                                    
            If Ptexto <> ComentarioW And ComentarioW <> *Blanks;                                    
             Wcbcmn = ComentarioW;                                                                  
             Pcmt2 = *Blanks;                                                                       
            Endif;                                                                                  
            If Ptexto = ComentarioW And ComentarioW <> *Blanks;                                     
             Wcbcmn = ComentarioW;                                                                  
             Pcmt2 = *Blanks;                                                                       
            Endif;                                                                                  
           Endif;                                                                                   
           If Wcbcmn = *Blanks;                                                                     
            Wcbcmn = %Trim(Texto) + ' ' +  %Trim(%Editc(Wcbval:'P'));                               
           Endif;                                                                                   
          Monitor;                                                                                  
          Clear Ofecha;                                                                             
          Ofecha = WaÑo + Wmes + Wdia;                                                              
          Dateln0 = %Date(%Trim(Ofecha):*Iso0);                                                     
          Dateln1 = %Date(%Trim(P1fec):*Iso0);                                                      
          Dias = %Diff(Dateln0:Dateln1:*Days);                                                      
          If Dias >= 2;                                                                             
           Wcbfet = WaÑo + '/' + Wmes + '/' + Wdia;                                                 
          Else;                                                                                     
           Wcbfet = %Subst(P1fec:1:4) + '/' + %Subst(P1fec:5:2) +                                   
                    '/' + %Subst(P1fec:7:2);                                                        
          Endif;                                                                                    
          On-Error;                                                                                 
           Wcbfet = WaÑo + '/' + Wmes + '/' + Wdia;                                                 
          Endmon;                                                                                   
                                                                                                    
           //WaÑo = %Subst(%Trim(FecTra):1:4);                                                      
           //Wmes = %Subst(%Trim(FecTra):5:2);                                                      
           //Wdia = %Subst(%Trim(FecTra):7:2);                                                      
           //Wcbfet = WaÑo + '/' + Wmes + '/' + Wdia;                                               
                                                                                                    
           Wcbsts = *Blanks;                                                                        
           Wcbusr = User;                                                                           
           Wcbcct = *Zeros;                                                                         
           Write Rwcab2;                                                                            
           BorrarTablaTmp();                                                                        
           Clear Posxx;                                                                             
           Clear Posyy;                                                                             
           Clear Poszz;                                                                             
           Posxx =  %Len(%Trim(P1ced));                                                             
           Posyy = LngFile;                                                                         
           Poszz = (Posyy - Posxx) + 1;                                                             
           If Pcia = '600';                                                                         
           If KKsw = 'N';                                                                           
              //If P2nroo = *Zeros;                                                                 
              // Arma la Estructura Requerida del Sql                                               
              If Poszz > 0 and Posxx > 20;                                                          
                 SqlNr1 ='Delete From Qgpl/' + %Trim(Pfile) +                                       
                 ' Where (Substr(trim(Cedula), ' + %Trim(%Editc(Poszz:'Z')) +                       
                 ', ' + %Trim(%Editc(Posxx:'Z')) + ') = Trim(' + '''' +                             
                 %Trim(P1ced)+ '''' + ') Or Trim(Cedula) = ' +                                      
                 'Trim(' + '''' + %Trim(Tced) +'''' + ') Or Trim(Cedula) = ' +                      
                 'Trim(' + '''' +%Trim(Tced) + '''' + ') '+                                         
                 ' Or Substr(trim(Cedula), ' + %Trim(%Editc(Poszz:'Z')) +                           
                 ', ' + %Trim(%Editc(Posxx:'Z')) + ') = ' +                                         
                 '''' + %Trim(P1ced) + '''' + ')' +                                                 
                 ' And Inder = ' + '''' + ' ' + '''' +                                              
                 ' And Trim(Valor) = ' + '''' +                                                     
                  %Trim(ValorX) + ''''+' with nc';                                                  
              Else;                                                                                 
           SqlNr1 ='Delete From Qgpl/' + %Trim(Pfile) + ' Where (Trim( +                            
          Cedula) = Trim(' + '''' + P1ced + '''' + ') Or Trim(Cedula) = ' +                         
          'Trim(' + '''' + %Trim(Tced) +'''' + ') Or Trim(Cedula) = ' +                             
          'Trim(' + '''' +%Trim(Tced) + '''' + ') Or Trim(Cedula) = ' +                             
          '''' + %Trim(P1ced) + '''' + ')' +                                                        
          ' And Inder = ' + '''' + ' ' + '''' +                                                     
          ' And Trim(Valor) = ' + '''' +                                                            
           %Trim(ValorX) + '''' +' with nc';                                                        
              endIf;                                                                                
           Else;                                                                                    
              If Poszz > 0 and Posxx > 20;                                                          
                 SqlNr1 ='Delete From Qgpl/' + %Trim(Pfile) +                                       
                 ' Where (Substr(trim(Cedula), ' + %Trim(%Editc(Poszz:'Z')) +                       
                 ', ' + %Trim(%Editc(Posxx:'Z')) + ') = Trim(' + '''' +                             
                 %Trim(P1ced)+'''' + ') Or Trim(Cedula) = ' +                                       
                 'Trim(' + '''' + %Trim(Tced) +'''' + ') Or Trim(Cedula) = ' +                      
                 'Trim(' + '''' +%Trim(Tced) + '''' + ') '+                                         
                 ' Or Substr(trim(Cedula), ' + %Trim(%Editc(Poszz:'Z')) +                           
                 ', ' + %Trim(%Editc(Posxx:'Z')) + ') = ' +                                         
                 '''' + %Trim(P1ced) + '''' + ')' +                                                 
                 ' And Inder = ' + '''' + ' ' + '''' +                                              
                 ' And Trim(Valor) = ' + '''' +                                                     
                  %Trim(ValorX) + '''' + ' And Rrn(' + %Trim(Pfile) + ') = ' +                      
                  %Trim(%Editc(Orrn:'Z')) + ' with nc';                                             
              Else;                                                                                 
           SqlNr1 ='Delete From Qgpl/' + %Trim(Pfile) + ' Where (Trim( +                            
          Cedula) = Trim(' + '''' + P1ced + '''' + ') Or Trim(Cedula) = ' +                         
          'Trim(' + '''' + %Trim(Tced) +'''' + ') Or Trim(Cedula) = ' +                             
          'Trim(' + '''' +%Trim(Tced) + '''' + ') Or Trim(Cedula) = ' +                             
          '''' + %Trim(P1ced) + '''' + ')' +                                                        
          ' And Inder = ' + '''' + ' ' + '''' +                                                     
          ' And Trim(Valor) = ' + '''' +                                                            
           %Trim(ValorX) + '''' + ' And Rrn(' + %Trim(Pfile) + ') = ' +                             
           %Trim(%Editc(Orrn:'Z')) + ' with nc';                                                    
              EndIf;                                                                                
           Endif;                                                                                   
           Else;                                                                                    
              If Poszz > 0 and Posxx > 20;                                                          
                 SqlNr1 ='Delete From Qgpl/' + %Trim(Pfile) +                                       
                 ' Where (Substr(trim(Cedula), ' + %Trim(%Editc(Poszz:'Z')) +                       
                 ', ' + %Trim(%Editc(Posxx:'Z')) + ') = Trim(' + '''' +                             
                 %Trim(P1ced)+ '''' + ') Or Trim(Cedula) = ' +                                      
                 'Trim(' + '''' + %Trim(Tced) +'''' + ') Or Trim(Cedula) = ' +                      
                 'Trim(' + '''' +%Trim(Tced) + '''' + ') '+                                         
                 ' Or Substr(trim(Cedula), ' + %Trim(%Editc(Poszz:'Z')) +                           
                 ', ' + %Trim(%Editc(Posxx:'Z')) + ') = ' +                                         
                 '''' + %Trim(P1ced) + '''' + ')' +                                                 
                 ' And Inder = ' + '''' + ' ' + '''' +                                              
                 ' And Trim(Valor) = ' + '''' +                                                     
                        %Trim(ValorX) + '''' + ' with nc';                                          
              Else;                                                                                 
           SqlNr1 ='Delete From Qgpl/' + %Trim(Pfile) + ' Where (Trim( +                            
          Cedula) = Trim(' + '''' + P1ced + '''' + ') Or Trim(Cedula) = ' +                         
          'Trim(' + '''' + %Trim(Tced) +'''' + ') Or Trim(Cedula) = ' +                             
          'Trim(' + '''' +%Trim(Tced) + '''' + ') Or Trim(Cedula) = ' +                             
          '''' + %Trim(P1ced) + '''' + ')' +                                                        
          ' And Inder = ' + '''' + ' ' + '''' +                                                     
          ' And Trim(Valor) = ' + '''' +                                                            
                 %Trim(ValorX) + '''' + ' with nc';                                                 
              EndIf;                                                                                
           Endif;                                                                                   
                                                                                                    
         Exec Sql                                                                                   
          Prepare Cur From :SqlNr1;                                                                 
                                                                                                    
         Exec Sql                                                                                   
           Execute Cur;                                                                             
                                                                                                    
          Endif;                                                                                    
        Endif;                                                                                      
        Endif;                                                                                      
       EndFor;                                                                                      
                                                                                                    
       If Tsw <> *Blanks;                                                                           
        Wvari = *Blanks;                                                                            
        Wvari = 'SBMJOB CMD(CALL PGM(VOTREP00/VTNCBI2L) PARM(' +                                    
           '''' + Pcia + '''' + '))' + ' JOB(SUBIR_BANC) JOBQ(QGPL/QBATCH)';                        
        Cmd = %Trim(Wvari);                                                                         
        Callp(E) Qcmdexc(Cmd:%Size(Cmd));                                                           
        *In79 = *On;                                                                                
        Exfmt Rdato1;                                                                               
        *In79 = *Off;                                                                               
        Cancelar = *On;                                                                             
       Endif;                                                                                       
       Endif;                                                                                       
       Enddo;                                                                                       
       Else;                                                                                        
        *In76 = *On;                                                                                
        Exfmt Rdato1;                                                                               
        *In76 = *Off;                                                                               
       Endif;                                                                                       
      /End-Free                                                                                     
     PCartera          E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Actualiza Registro del Archivo Vttrbsf         //                       
       //------------------------------------------------------------------//                       
     PActualRegF       B                                                                            
     DActualRegF       Pi                                                                           
      /Free                                                                                         
        Chain (Pcia:%Int(Pcbnx):Pdarc) Vttrbsf;                                                     
         If %Found(Vttrbsf);                                                                        
          If PrVarlidarIncon() = *On;                                                               
           Rbsinp = 'T';                                                                            
          Else;                                                                                     
           Rbsinp = 'P';                                                                            
          Endif;                                                                                    
          Update Regrbs %Fields(Rbsinp);                                                            
         Endif;                                                                                     
      /End-Free                                                                                     
     PActualRegF       E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Verifica que Pagos Hay Duplicados              //                       
       //------------------------------------------------------------------//                       
     PPagosDupli       B                                                                            
     DPagosDupli       Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DSqlStr           s           1000a   Inz(*Blanks)                                             
     DSqlNr1           s            500a   Inz(*Blanks)                                             
     DNroCed           s              9s 0 Inz(1)                                                   
     DIdx              s              4s 0 Inz(1)                                                   
     DIdx2             s              4s 0 Inz(1)                                                   
     DECedula          s             30a   Inz(*Blanks)                                             
     DPunteroa         s              9b 0 Inz(*Zeros)                                              
     DCedulaAux        s             30a   Inz(*Blanks)                                             
     DValorAux         s            500a   Inz(*Blanks)                                             
     DCons             s              9s 0 Inz(*Zeros)                                              
     DVar0             s              1a   Inz(*Blanks)                                             
     DIconSec          s              9a   Inz(*Blanks)                                             
                                                                                                    
      ** Definición de Estructuras de Trabajo                                                       
     DFields           Ds                                                                           
     DCedulaT                        30a   Inz(*Blanks)                                             
     DValort                        500a   Inz(*Blanks)                                             
                                                                                                    
     DFields1          Ds                                                                           
     DCedulab                        30a   Inz(*Blanks)                                             
     DValorb                        500a   Inz(*Blanks)                                             
     DPunterob                        9b 0 Inz(*Zeros)                                              
                                                                                                    
      /Free                                                                                         
       SqlNr1 = *Blanks;                                                                            
       SqlNr1 = 'Select Distinct Cedula, Valor From Qgpl/' + %Trim(Pfile) +                         
                ' As T1 Where Cedula In (Select Cedula From Qgpl/' +                                
                 %Trim(Pfile) + ' As T2 where T1.Consec <> T2.Consec+                               
                ) And (Valor in (select Valor From Qgpl/' +                                         
                %Trim(Pfile) + ' As T2 Where T1.Consec <> T2.Consec))' +                            
               ' Order By Cedula, Valor';                                                           
       SqlNr1 = %trim(SqlNr1)+' with nc';                                                           
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur10 From :SqlNr1;                                                                 
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor10 Dynamic Scroll Cursor For Cur10;                                           
                                                                                                    
       Exec Sql                                                                                     
        Open Cursor10;                                                                              
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor10 Into :Fields;                                                                
                                                                                                    
        Dow Sqlcod <> 100 And Sqlcod >= *Zeros;                                                     
         VecCedula(Idx) = CedulaT;                                                                  
         Idx += 1;                                                                                  
         Exec Sql                                                                                   
          Fetch Cursor10 Into :Fields;                                                              
        Enddo;                                                                                      
                                                                                                    
       Exec Sql                                                                                     
        Close Cursor10;                                                                             
                                                                                                    
        For Idx2 = 1 To Idx;                                                                        
         SqlNr1 = *Blanks;                                                                          
         SqlNr1 = 'Select Cedula, Valor, Consec ' +                                                 
                  ' From Qgpl/' + %Trim(Pfile) +                                                    
                  ' Where Cedula = ' + '''' + %Trim(VecCedula(Idx2)) + ''''                         
               +  ' Order By Cedula, Valor';                                                        
         SqlNr1 = %trim(SqlNr1)+' with nc';                                                         
                                                                                                    
        Exec Sql                                                                                    
         Prepare Cur11 From :SqlNr1;                                                                
                                                                                                    
        Exec Sql                                                                                    
         Declare Cursor11 Dynamic Scroll Cursor For Cur11;                                          
                                                                                                    
        Exec Sql                                                                                    
         Open Cursor11;                                                                             
                                                                                                    
        Exec Sql                                                                                    
         Fetch Cursor11 Into :Fields1;                                                              
                                                                                                    
         NroCed = 1;                                                                                
         Dow Sqlcod <> 100 And SqlCod >= *Zeros;                                                    
         If NroCed > 1;                                                                             
                                                                                                    
         SqlStr = *Blanks;                                                                          
         SqlStr = 'Update Qgpl/' + %Trim(Pfile) + ' Set Inder =' +                                  
                 '''' + 'I' + '''' + ' Where Cedula = ' + '''' +                                    
                 %Trim(VecCedula(Idx2)) + '''' + ' And Consec <> ' +                                
                 %Trim(IconSec) + ' And Valor = ' +                                                 
                 %Trim(ValorAux) +  ' And Inder <> ''  ''';                                         
         SqlStr = %trim(SqlStr)+' with nc';                                                         
                                                                                                    
         Exec Sql                                                                                   
          Prepare Cur15 From :SqlStr;                                                               
                                                                                                    
         Exec Sql                                                                                   
           Execute Cur15;                                                                           
                                                                                                    
         Else;                                                                                      
          Punteroa = Punterob;                                                                      
          IconSec = %Trim(%Editc(Punterob:'Z'));                                                    
         Endif;                                                                                     
         If Cedulab = CedulaAux And Valorb = ValorAux;                                              
          NroCed += 1;                                                                              
         Else;                                                                                      
          CedulaAux = Cedulab;                                                                      
          ValorAux = Valorb;                                                                        
          Cons = Punterob;                                                                          
         Endif;                                                                                     
          Exec Sql                                                                                  
           Fetch Cursor11 Into :Fields1;                                                            
        Enddo;                                                                                      
                                                                                                    
        Exec Sql                                                                                    
         Close Cursor11;                                                                            
        Endfor;                                                                                     
      /End-Free                                                                                     
     PPagosDupli       E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Borra Registro del Sub File                    //                       
       //------------------------------------------------------------------//                       
     PBorrarRegSfl     B                                                                            
     DBorrarRegSfl     Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DSqlNr1           s            500a   Inz(*Blanks)                                             
      /Free                                                                                         
       Clear Rdato3;                                                                                
       Dow Popc = *Blanks;                                                                          
        Exfmt Rdato3;                                                                               
        If Popc = 's' Or Popc = 'S';                                                                
                                                                                                    
         SqlNr1 ='Delete From Qgpl/' + %Trim(Pfile) + ' Where Trim(+                                
                  Cedula) = ' + '''' +  %Trim(P1ced) + '''' +                                       
                  ' And ' + 'Consec = ' + %Char(Pccon) + ' with nc';                                
                                                                                                    
         Exec Sql                                                                                   
          Prepare Cur From :SqlNr1;                                                                 
                                                                                                    
         Exec Sql                                                                                   
           Execute Cur;                                                                             
                                                                                                    
          *In39 = *On;                                                                              
          Exfmt Rdato3;                                                                             
          *In39 = *Off;                                                                             
        Endif;                                                                                      
       Enddo;                                                                                       
      /End-Free                                                                                     
     PBorrarRegSfl     E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Actualiza Registro del Archivo Vttrbsf         //                       
       //------------------------------------------------------------------//                       
     PMoverFile        B                                                                            
     DMoverFile        Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DP                s              4s 0 Inz(*Zeros)                                              
     DXarcr            s            100a   Inz(*Blanks)                                             
     DXarch            s            100a   Inz(*Blanks)                                             
     DXexte            s              4a   Inz(*Blanks)                                             
     DXruta            s            200a   Inz(*Blanks)                                             
     DXruta1           s            200a   Inz(*Blanks)                                             
      /Free                                                                                         
       Chain(n) (Pcia:%Int(Pcbnx):Pdarc) Vttrbsf;                                                   
       If %Found(Vttrbsf);                                                                          
        Clear P;                                                                                    
        P = %Scan('.':Rbsnar);                                                                      
       If P > *Zeros;                                                                               
        Xarcr = %Subst(Rbsnar:1:(p-1)) + %Subst(Rbsnar:p:6);                                        
        Xarch = '/QDLS/CIA' + %Trim(Pcia) + '/BANCOS/GRABADOS/' +                                   
                %Trim(Rbsnar);                                                                      
        Xruta = %Trim(Xarch);                                                                       
        Xruta1 = '/QDLS/CIA' + %Trim(Pcia) + '/BANCOS/RECIBIR/' +                                   
                %Trim(Rbsnar);                                                                      
                                                                                                    
        Wvari = *Blanks;                                                                            
        Wvari = 'MOVE OBJ(' + '''' + %Trim(Xruta1) + '''' + ')' + ' ' +                             
                'TOOBJ(' + '''' + %Trim(Xruta) + '''' + ')';                                        
        Cmd = %Trim(Wvari);                                                                         
        Callp(E) Qcmdexc(Cmd:%Size(Cmd));                                                           
       Endif;                                                                                       
                                                                                                    
       Endif;                                                                                       
      /End-Free                                                                                     
     PMoverFile        E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Genera Informe de Inconsistencias de Pagos     //                       
       //------------------------------------------------------------------//                       
     PInformeIncons    B                                                                            
     DInformeIncons    Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DNrri             s              4s 0 Inz(*Zeros)                                              
     DNrrf             s              4s 0 Inz(1)                                                   
      /Free                                                                                         
       If Nrr > *Zeros And Psw = 'S';                                                               
        Chain (Pcia:%Int(Pcbnx)) Vttrbsl1;                                                          
        Dow Not %Eof(Vttrbsl1);                                                                     
        If %Found(Vttrbsl1) And (aRbsini = *Blanks                                                  
        Or aRbsini = 'G') And aRbsinp = 'P';                                                        
        *In55 = *On;                                                                                
        NroInco = *Zeros;                                                                           
        TitulosInf();                                                                               
        Nrri = Nrr;                                                                                 
        Dow Nrrf <= Nrri;                                                                           
         Chain Nrrf Rdatos1;                                                                        
         If P1men <> *Blanks;                                                                       
          DetalleInf();                                                                             
         Endif;                                                                                     
         Nrrf += 1;                                                                                 
        Enddo;                                                                                      
        TotalInf();                                                                                 
        aRbsini = 'G';                                                                              
        Update Rgrbs %Fields(aRbsini);                                                              
        Leave;                                                                                      
        Endif;                                                                                      
       ReadE (Pcia:%Int(Pcbnx)) Vttrbsl1;                                                           
       Enddo;                                                                                       
       Endif;                                                                                       
      /End-Free                                                                                     
     PInformeIncons    E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Imprime los Titulos del Informe                //                       
       //------------------------------------------------------------------//                       
     PTitulosInf       B                                                                            
     DTitulosInf       Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DNrri             s              4s 0 Inz(*Zeros)                                              
      /Free                                                                                         
       If *In55 = *On;                                                                              
        *In55 = *Off;                                                                               
        Ipgm = 'VTMCCC9L';                                                                          
        Fechai= %Char(%Date);                                                                       
        Ibanco = Xbanco;                                                                            
        Write Titulos;                                                                              
       Endif;                                                                                       
      /End-Free                                                                                     
     PTitulosInf       E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Imprime el Detalle del Informe                 //                       
       //------------------------------------------------------------------//                       
     PDetalleInf       B                                                                            
     DDetalleInf       Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DNrri             s              4s 0 Inz(*Zeros)                                              
      /Free                                                                                         
        Iced = P1ced;                                                                               
        Iced1 = '_______________';                                                                  
        Iorden = P1ord;                                                                             
        Ivalor = P1val;                                                                             
        Ifecha = P1fec;                                                                             
        SqlNr1 = *Blanks;                                                                           
        // Campo Adicional 1                                                                        
        SqlNr1 = 'Select Camad1' +                                                                  
                 ' From Qgpl/' + %Trim(Pfile) + ' Where Cedula =' + '''' +                          
                 %Trim(P1ced) + '''';                                                               
        SqlNr1 = %trim(SqlNr1)+' with nc';                                                          
                                                                                                    
         Exec Sql                                                                                   
          Prepare Cur8 From :SqlNr1;                                                                
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor8 Dynamic Scroll Cursor For Cur8;                                             
                                                                                                    
       Exec Sql                                                                                     
        Open Cursor8;                                                                               
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor8 Into :Camad1a;                                                                
                                                                                                    
        If Sqlcod <> 100;                                                                           
         Iofic = Camad1a;                                                                           
        Else;                                                                                       
         Iofic = *Blanks;                                                                           
        EndIf;                                                                                      
                                                                                                    
       Exec Sql                                                                                     
        Close Cursor8;                                                                              
                                                                                                    
        // Campo Adicional 2                                                                        
        SqlNr1 = *Blanks;                                                                           
        SqlNr1 = 'Select Camad2' +                                                                  
              ' From Qgpl/' + %Trim(Pfile) + ' Where Cedula =' + '''' +                             
               %Trim(P1ced) + '''';                                                                 
        SqlNr1 = %trim(SqlNr1)+' with nc';                                                          
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur9 From :SqlNr1;                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor9 Dynamic Scroll Cursor For Cur9;                                             
                                                                                                    
       Exec Sql                                                                                     
        Open Cursor9;                                                                               
                                                                                                    
         Exec Sql                                                                                   
         Fetch Cursor9 Into :Camad2a;                                                               
                                                                                                    
        If Sqlcod <> 100;                                                                           
         Icodba = Camad2a;                                                                          
        Else;                                                                                       
         Icodba = *Blanks;                                                                          
        EndIf;                                                                                      
                                                                                                    
        Exec Sql                                                                                    
         Close Cursor9;                                                                             
                                                                                                    
        Icomen = Ptexto;                                                                            
        Incon = P1men;                                                                              
        NroInco += 1;                                                                               
        If *In55 = *On;                                                                             
         TitulosInf();                                                                              
        Endif;                                                                                      
        Write Detalle;                                                                              
      /End-Free                                                                                     
     PDetalleInf       E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Imprime los Totales del Informe                //                       
       //------------------------------------------------------------------//                       
     PTotalInf         B                                                                            
     DTotalInf         Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DNrri             s              4s 0 Inz(*Zeros)                                              
      /Free                                                                                         
        Totalw = NroInco;                                                                           
        Write Totales;                                                                              
      /End-Free                                                                                     
     PTotalInf         E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Borra el Archivo Creado en la Qgpl Despues de  //                       
       // Subir el Banco y Postear los Pagos a la Cartera                  //                       
       //------------------------------------------------------------------//                       
     PBorrarFileQgpl   B                                                                            
     DBorrarFileQgpl   Pi                                                                           
     DPfileYY                        10a                                                            
      ** Definición de Variables de Trabajo                                                         
     DNroreg           s              5s 0 Inz(*Zeros)                                              
     DKres             s              2a   Inz('No')                                                
      /Free                                                                                         
        Clear NroReg;                                                                               
        Kres = 'No';                                                                                
        KresG = 'No';                                                                               
        SqlNr1 = *Blanks;                                                                           
        SqlNr1 = 'Select Count(0) As NroReg' +                                                      
                 ' From Qgpl/' + %Trim(PfileYY)+ ' with nc';                                        
                                                                                                    
        Exec Sql                                                                                    
         Prepare Cur15 From :SqlNr1;                                                                
                                                                                                    
        Exec Sql                                                                                    
         Declare Cursor15 Cursor For Cur15;                                                         
                                                                                                    
        Exec Sql                                                                                    
         Open Cursor15;                                                                             
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor15 Into :NroReg;                                                                
                                                                                                    
        If Sqlcod <> 100 And Sqlcod >= *Zeros;                                                      
         If NroReg <= *Zeros;                                                                       
          Kres = 'Si';                                                                              
         Endif;                                                                                     
        ElseIf Sqlcod = 100;                                                                        
         Kres = 'Si';                                                                               
        EndIf;                                                                                      
        KresG = Kres;                                                                               
                                                                                                    
       Exec Sql                                                                                     
        Close Cursor15;                                                                             
                                                                                                    
       If Kres = 'Si';                                                                              
        BorrarRegF();                                                                               
        Wvari = *Blanks;                                                                            
        Wvari = 'Dltobj Obj(Qgpl/' + %Trim(PfileYY) + ')'  +                                        
                ' Objtype(*File)';                                                                  
        Cmd = %Trim(Wvari);                                                                         
        Monitor;                                                                                    
         Qcmdexc(Cmd:%Size(Cmd));                                                                   
        On-Error *All;                                                                              
        Endmon;                                                                                     
        Clear SqlNr1;                                                                               
        SqlNr1 = 'Drop table Qgpl/' + %Trim(PfileYY);                                               
         Exec Sql                                                                                   
          Prepare Cur From :SqlNr1;                                                                 
         Exec Sql                                                                                   
           Execute Cur;                                                                             
       Endif;                                                                                       
        Clear SqlNr1;                                                                               
        SqlNr1= 'VTMCCC9R ' +PfileYY+' SQLCOD=' +%char(Sqlcod) +                                    
                %Trim(Wvari )+'--'+%Trim(Kres)+'NunReg:'+%Char(NroReg);                             
        Exec sql                                                                                    
        Insert Into  GNPNALSOF4/PRUEBALE (REGISTRO) Values (:SqlNr1);                               
      /End-Free                                                                                     
     PBorrarFileQgpl   E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Borra el Archivo Creado en la Qgpl Despues de  //                       
       // Subir el Banco y Postear Todos los Pagos de la Cartera           //                       
       //------------------------------------------------------------------//                       
     PBorrarFileQgplTmp...                                                                          
     P                 B                                                                            
     DBorrarFileQgplTmp...                                                                          
     D                 Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DNroregX          s              5s 0 Inz(*Zeros)                                              
     DKresX            s              2a   Inz('No')                                                
      /Free                                                                                         
        SqlNr1 = *Blanks;                                                                           
        SqlNr1 = 'Select Count(0) As NroRegX' +                                                     
                 ' From Qgpl/PgsOrd' +                                                              
                 ' Wpais = ' + '''' + %Trim(Pcia) + ''''+ ' with nc';                               
                                                                                                    
        Exec Sql                                                                                    
         Prepare Cur16 From :SqlNr1;                                                                
                                                                                                    
        Exec Sql                                                                                    
         Declare Cursor16 Cursor For Cur16;                                                         
                                                                                                    
        Exec Sql                                                                                    
         Open Cursor16;                                                                             
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor16 Into :NroRegX;                                                               
                                                                                                    
        KresX = 'No';                                                                               
        If Sqlcod <> 100;                                                                           
         If NroRegX <= *Zeros;                                                                      
          KresX = 'Si';                                                                             
         Endif;                                                                                     
        EndIf;                                                                                      
                                                                                                    
       Exec Sql                                                                                     
        Close Cursor16;                                                                             
                                                                                                    
       If KresX = 'Si';                                                                             
        Wvari = *Blanks;                                                                            
        Wvari = 'Dltobj Obj(Qgpl/PgsOrd)'  +                                                        
                ' Objtype(*File)';                                                                  
        Cmd = %Trim(Wvari);                                                                         
        Callp(E) Qcmdexc(Cmd:%Size(Cmd));                                                           
       Endif;                                                                                       
      /End-Free                                                                                     
     PBorrarFileQgplTmp...                                                                          
     P                 E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Acepta Pagos Dobles Enviados por el Banco      //                       
       //------------------------------------------------------------------//                       
     PAcepPagosDobles  B                                                                            
     DAcepPagosDobles  Pi                                                                           
      /Free                                                                                         
        SqlStr = *Blanks;                                                                           
        SqlStr = 'Update Qgpl/' + %Trim(Pfile) + ' Set Inder =' +                                   
                 '''' + ' ' + '''' + ' Where Cedula = ' + '''' +                                    
                 %Trim(P2ced) + '''' + ' And Consec = ' +                                           
                 %Trim(%Editc(Pccons:'Z')) +' with nc ';                                            
                                                                                                    
         Exec Sql                                                                                   
          Prepare Cur19 From :SqlStr;                                                               
                                                                                                    
         Exec Sql                                                                                   
           Execute Cur19;                                                                           
      /End-Free                                                                                     
     PAcepPagosDobles  E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Calcula el Saldo de una Compradora             //                       
       //------------------------------------------------------------------//                       
     PCalSlado         B                                                                            
     DCalSlado         Pi                                                                           
      /Free                                                                                         
       Chain(n) CedulaR Parsdow;                                                                    
       If %Found(Parsdow);                                                                          
        If Adcsdo = *Zeros;                                                                         
         Hsw = 'S';                                                                                 
        Endif;                                                                                      
       Else;                                                                                        
         Hsw = 'S';                                                                                 
       Endif;                                                                                       
      /End-Free                                                                                     
     PCalSlado         E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Borra Registro del Archivo Vttrbsf             //                       
       //------------------------------------------------------------------//                       
     PBorrarRegF       B                                                                            
     DBorrarRegF       Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DPfechaX          s             10a   Inz(*Blanks)                                             
      /Free                                                                                         
        PfechaX = %Char(%Date());                                                                   
       // Chain (Pcia:%Int(Pcbnx):Pdarc) Vttrbsf;                                                   
       //  If %Found(Vttrbsf);                                                                      
       //   Rbsinp = 'T';                                                                           
       //   Update Regrbs;                                                                          
       //   Endif;                                                                                  
                                                                                                    
        Exec Sql                                                                                    
         Delete From Qgpl/PgsOrd Where Trim(Wfile) = Trim(:Pfile)                                   
         And Wpais = :Pcia And Wfecha = :PfechaX;                                                   
                                                                                                    
      /End-Free                                                                                     
     PBorrarRegF       E                                                                            
                                                                                                    
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
          Cmd = 'SNDBRKMSG  MSG(' + '''' +'La opción de posteo se esta ' +                          
                'ejecutado por otro usuario.. Intente más ' +                                       
                'tarde' + '''' + ') TOMSGQ(' + %Trim(Job) + ')';                                    
          Callp(E) Qcmdexc(Cmd:%Size(Cmd));                                                         
        EndSl;                                                                                      
       Out MyDtaara;                                                                                
       Return ProcesoW;                                                                             
      /End-Free                                                                                     
     PPrDtaara         E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Consulta si hay Inconsistencias Pendientes     //                       
       //------------------------------------------------------------------//                       
     PPrVarlidarIncon...                                                                            
     P                 B                                                                            
     DPrVarlidarIncon...                                                                            
     D                 Pi              n                                                            
      ** Definición de Variables de Trabajo                                                         
     DProcesoW         s               n   Inz(*Off)                                                
     DNroRtt           s              9s 0 Inz(*Zeros)                                              
      /Free                                                                                         
       SqlNr1 = *Blanks;                                                                            
       SqlNr1 = 'Select Count(0) As NroRtt' +                                                       
                ' From Qgpl/' + %Trim(Rbsarc)+' with nc';                                           
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur15a From :SqlNr1;                                                                
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor15a Cursor For Cur15a;                                                        
                                                                                                    
       Exec Sql                                                                                     
        Open Cursor15a;                                                                             
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor15a Into :NroRtt;                                                               
                                                                                                    
       If Sqlcod <> 100 And Sqlcod >= *Zeros;                                                       
        //If NroRtt > *Zeros;                                                                       
        If NroRtt <= *Zeros;                                                                        
         ProcesoW = *On;                                                                            
        Endif;                                                                                      
       Endif;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close Cursor15a;                                                                            
                                                                                                    
       Return ProcesoW;                                                                             
      /End-Free                                                                                     
     PPrVarlidarIncon...                                                                            
     P                 E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Consulta si hay Inconsistencias Pendientes     //                       
       //------------------------------------------------------------------//                       
     PPrComentarioValor...                                                                          
     P                 B                                                                            
     DPrComentarioValor...                                                                          
     D                 Pi                                                                           
     DValorD                        100a                                                            
      /Free                                                                                         
        KKsw = 'N';                                                                                 
        SqlNrX = *Blanks;                                                                           
        // Campo Adicional 1                                                                        
        SqlNrX = 'Select Camad1, Rrn(' + %Trim(Pfile) + '), Trim(Valor) ' +                         
                 'From Qgpl/' + %Trim(Pfile) + ' Where Cedula =' + '''' +                           
                 %Trim(P1ced) + '''' + ' And Valor= ' + '''' +                                      
                 %Trim(ValorD) + ''''+' with nc';                                                   
                                                                                                    
         Exec Sql                                                                                   
          Prepare CurX600 From :SqlNrX;                                                             
                                                                                                    
       Exec Sql                                                                                     
        Declare CursorX600 Dynamic Scroll Cursor For CurX600;                                       
                                                                                                    
       Exec Sql                                                                                     
        Open CursorX600;                                                                            
                                                                                                    
       Exec Sql                                                                                     
        Fetch CursorX600 Into :Camad1x, :Orrn, :Povalor;                                            
                                                                                                    
       If Sqlcod = 100 Or SqlCod < *Zeros;                                                          
        KKsw = 'N';                                                                                 
        Camad1x = *Blanks;                                                                          
       ElseIf Sqlcod <> 100 And SqlCod >= *Zeros;                                                   
        KKsw = 'S';                                                                                 
       Endif;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close CursorX600;                                                                           
      /End-Free                                                                                     
     PPrComentarioValor...                                                                          
     P                 E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Verifica Pagos del WebStore Compañia 003       //                       
       //------------------------------------------------------------------//                       
     PPrPgosWebStore   B                                                                            
     DPrPgosWebStore   Pi              n                                                            
      ** Definición de Variables de Trabajo                                                         
     DPsw              s               n   Inz(*Off)                                                
     DTcnsW            s              9    Inz(*Blanks)                                             
     DTcedW            s             30    Inz(*Blanks)                                             
     DTvalor           s             11  2 Inz(*Zeros)                                              
     DTvalorA          s             11  0 Inz(*Zeros)                                              
     DNpgo             s              9  0 Inz(*Zeros)                                              
     DNroRR            s              9  0 Inz(*Zeros)                                              
     DValorXA          s            100a   Inz(*Blanks)                                             
      /Free                                                                                         
       If Pcia <= '002';                                                                            
        If %Len(%Trim(P1val)) > *Zeros;                                                             
         ValorXA = *Blanks;                                                                         
         If %Len(%Trim(P2val)) > *Zeros;                                                            
          ValorXA = %Trim(P2val);                                                                   
         Else;                                                                                      
          ValorXA = %Trim(P1val);                                                                   
         Endif;                                                                                     
         Tvalor = %Dec(%Trim(P1val):11:2) * (-1);                                                   
         TvalorA = %Int(%Trim(P1val)) * (-1);                                                       
         TcnsW = *Blanks;                                                                           
         TcedW = %Trim(P1ced);                                                                      
         Conversion(Pcia:TcedW:TcnsW);                                                              
         // Se Realiza Consulta si el pago esta en la Compañia 003                                  
         Npgo = *Zeros;                                                                             
         Exec Sql                                                                                   
          Select Count(0), Max(Rrn(Vttpewf)) Into :Npgo, :NroRR                                     
          From Votrea00/Vttpewf                                                                     
          Where Pewcia = '003' And                                                                  
               ((Trim(Pewco1) = Trim(:TcnsW) Or Trim(Pewco1) = Trim(:TcedW)) Or                     
       (Trim(Char(Pewoae)) = Trim(:Tcnsw) Or Trim(Char(Pewoae)) = Trim(:Tcedw)))                    
            And :Tvalor Between (Pewval-100) And (Pewval+100) And Pewpap = 'P';                     
                                                                                                    
         If Npgo > *Zeros;                                                                          
          Psw = *On;                                                                                
          Exec Sql                                                                                  
           Update Votrea00/Vttpewf Set Pewfpa = Current Date,                                       
                                       Pewhpa = Current Time,                                       
                                       Pewpap = 'R',                                                
                                       Pewusr= 'DLM Efecty',                                        
                                       Pewtie = 'Pago Via DLM'                                      
           Where Pewcia = '003' And                                                                 
                 (Trim(Pewco1) = Trim(:TcnsW) Or Trim(Pewco1) = Trim(:TcedW) Or                     
        Trim(Char(Pewoae)) = Trim(:Tcnsw) Or Trim(Char(Pewoae)) = Trim(:Tcedw))                     
            And :Tvalor Between (Pewval-100) And (Pewval+100) And Pewpap = 'P'                      
            And Rrn(Vttpewf) = :NroRR;                                                              
                                                                                                    
          SqlNr1 = *Blanks;                                                                         
          SqlNr1 ='Delete From Qgpl/' + %Trim(Pfile) + ' Where (Trim( +                             
             Cedula) = Trim(' + '''' + TcnsW + '''' + ') Or Trim(Cedula) = ' +                      
             'Trim(' + '''' + %Trim(TcedW) +'''' + ') Or Trim(Cedula) = ' +                         
             'Trim(' + '''' +%Trim(TcedW) + '''' + ') Or Trim(Cedula) = ' +                         
             '''' + %Trim(TcnsW) + '''' + ')' +                                                     
             ' And Inder = ' + '''' + ' ' + '''' +                                                  
             ' And (Int(Trim(Valor)) = ' + '''' +                                                   
             %Trim(%Editc(TvalorA:'Z')) + '''' + ' Or Trim(Valor) = ' +                             
             '''' + %Trim(ValorXA) + '''' + ') with nc';                                            
                                                                                                    
          Exec Sql                                                                                  
           Prepare CurNew From :SqlNr1;                                                             
                                                                                                    
          Exec Sql                                                                                  
            Execute CurNew;                                                                         
                                                                                                    
         Endif;                                                                                     
        Endif;                                                                                      
       Endif;                                                                                       
                                                                                                    
       Return Psw;                                                                                  
      /End-Free                                                                                     
     PPrPgosWebStore   E                                                                            
