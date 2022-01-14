     **                                                                   *                         
     ** Programa      : VTMCCC4R                                          *                         
     ** Descripciòn:  : Cargar Archivos Bancos Opi                        *                         
     ** Proyecto      : Menú de Opciones Opi Paises                       *                         
     ** Autor         : (Jlopezl)                                         *                         
     ** Fecha Creaciòn: 13 de Enero de 2009                               *                         
     **                                                                   *                         
     **********************************************************************                         
     ** Definición de Directivas de Compilación                                                     
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
     ***************************************************                                            
                                                                                                    
     FVtmbncf   If   e           k Disk    usropn                                                   
     FVttbcpf   if   e           k Disk    usropn                                                   
     FVttbcsf   if   e           k Disk    usropn                                                   
     FVtmcaof   if   e           k Disk    usropn                                                   
     FVttrbsf   Uf a e           k Disk                                                             
     FVtwpbcf   if a e           k Disk                                                             
                                                                                                    
     ** Definiciòn de Archivo de Pantallas                                                          
     FVtbnci1p  cf   e             Workstn                                                          
     F                                     Sfile(Rdatos1:Nrr1)                                      
     F                                     Sfile(Rdatos2:Nrr2)                                      
                                                                                                    
                                                                                                    
     ***************************************************                                            
     DPosix            s              2s 0 Inz(*Zeros)                                              
     DAplicacion       s              3a   Inz('BAN')                                               
     DFile             s             20a   Inz('Blanks')                                            
     DRuta             s             64a   Inz('Blanks')                                            
     DNrr2             s              4s 0 Inz(*Zeros)                                              
     DPdcbn1           s              2s 0 Inz(*Zeros)                                              
     DXreg             s           1000    Inz(*Zeros)                                              
     Dmseconds         s              6s 0 inz                                                      
     Dmsecondsa        s              6a   Inz(*Blanks)                                             
                                                                                                    
     D                 DS                                                                           
     DSiglo                    1      2S 0                                                          
     DAÑo                      3      4S 0                                                          
     DMes                      5      6S 0                                                          
     DDia                      7      8S 0                                                          
     DFecha                    1      8S 0                                                          
                                                                                                    
     DXcia             s              3    Inz(*Blanks)                                             
     DXpais            s             20    Inz(*Blanks)                                             
     DFsw              s              1a   Inz(*Blanks)                                             
                                                                                                    
     DFecha2          sds                                                                           
     DSig_sys                195    196  0                                                          
     DAÑo_sys                280    281  0                                                          
     DMes_sys                276    277  0                                                          
     DDia_sys                278    279  0                                                          
     DUser_Name              254    263                                                             
                                                                                                    
     DCmd              s             20                                                             
     DLong             s             15p 5                                                          
     DComando          Pr                  Extpgm('QCMDEXC')                                        
     D                               20    Options(*Varsize)                                        
     D                                     Const                                                    
     DL                              15p 5                                                          
                                                                                                    
     DPgmPais          Pr                  Extpgm('VOTREP00/VTMCSBBR')                              
     DXcia                            3                                                             
     DXpais                          20                                                             
                                                                                                    
     DPgmRutaF         Pr                  Extpgm('VOTREP00/VTMCCCAR')                              
     DXcia                            3                                                             
     DXRuta                          64                                                             
     DXfile                          20                                                             
     DXreg                         1000                                                             
                                                                                                    
     DFilePar          Pr                                                                           
     DGrabarReg        Pr                                                                           
                                                                                                    
     DXruta            s             64                                                             
     DXfile            s             20                                                             
     DXcdbn            s              2                                                             
     DXcdes            s              3                                                             
     DXtpar            s              1                                                             
     DXtres            s              1                                                             
     DSubirFile        Pr                  Extpgm('VOTREP00/VTMCCC8L')                              
     DXcia                            3                                                             
     DXruta                          64                                                             
     DXfile                          20                                                             
     DXcdbn                           2                                                             
     DXcdes                           3                                                             
     DXtpar                           1                                                             
     DXtres                           1                                                             
                                                                                                    
     DRfile            s             20a   inz(*Blanks)                                             
     DPgmRuta          Pr                  Extpgm('VOTREP00/VTMCCC6L')                              
     DRcia                            3                                                             
     DRRuta                          64                                                             
     DRfile                          20                                                             
     D                                                                                              
                                                                                                    
      /Free                                                                                         
           PgmPais(Compania:Xpais);                                                                 
           dow *in03 = *Off;                                                                        
            If Posix > *Zeros;                                                                      
             Pbnc  = Posix;                                                                         
            Endif;                                                                                  
            Ppais = Xpais;                                                                          
            *In80 = *On;                                                                            
            Write RDato2;                                                                           
            Exfmt RDato5;                                                                           
            *In80 = *Off;                                                                           
            Fsw = *Blanks;                                                                          
                                                                                                    
            If *In03 = *On;                                                                         
             Leave;                                                                                 
            Endif;                                                                                  
                                                                                                    
            If *In06 = *On And Pdbnc <> *Blanks And Pdestr <> *Blanks And                           
             Pdarch <> *Blanks;                                                                     
             Xcia = Compania;                                                                       
             Xruta = Ruta;                                                                          
             Xfile = Rfile;                                                                         
             PgmRutaF(Xcia:Xruta:Xfile:Xreg);                                                       
             Chain (Compania:Pdcbn1:Xreg) Vttrbsf;                                                  
             If Not %Found(Vttrbsf);                                                                
              Evalr Xcdbn = %Char(Pdcbn1);                                                          
              Evalr Xcdes = %Char(Pces);                                                            
              //Mseconds = (%SubDt(%Timestamp():*MS) / 1000);                                       
              //Msecondsa = %Subst(%Char(Mseconds):1:2);                                            
              //Evalr Xcdes = Msecondsa;                                                            
              Xtpar = Tipoestr;                                                                     
              FilePar();                                                                            
              Xtres = *Blanks;                                                                      
              If Xtres = *Blanks;                                                                   
               GrabarReg();                                                                         
              Endif;                                                                                
              SubirFile(Xcia:Xruta:Xfile:Xcdbn:Xcdes:Xtpar:Xtres);                                  
             *In03 = *On;                                                                           
             Else;                                                                                  
          // If Rbsinp = 'T';                                                                       
              *In55 = *On;                                                                          
              Exfmt Rdato1;                                                                         
              *In55 = *Off;                                                                         
          // Else;                                                                                  
          //  Evalr Xcdbn = %Char(Pdcbn1);                                                          
          //  Evalr Xcdes = %Char(Pces);                                                            
          //  Xtpar = Tipoestr;                                                                     
          //  FilePar();                                                                            
          //  Xtres = *Blanks;                                                                      
          //  If Xtres = *Blanks;                                                                   
          //   GrabarReg();                                                                         
          //  Endif;                                                                                
          //  SubirFile(Xcia:Xruta:Xfile:Xcdbn:Xcdes:Xtpar:Xtres);                                  
          // *In03 = *On;                                                                           
          // Endif;                                                                                 
            Endif;                                                                                  
            Endif;                                                                                  
                                                                                                    
            If *In12 = *On;                                                                         
             Clear Rdato5;                                                                          
             Iter;                                                                                  
            Endif;                                                                                  
                                                                                                    
             If %Open (Vtmbncf) = '0';                                                              
              Open Vtmbncf;                                                                         
             Endif;                                                                                 
                                                                                                    
             If %Open (Vtmcaof) = '0';                                                              
              Open Vtmcaof;                                                                         
             Endif;                                                                                 
                                                                                                    
             If %Open (Vttbcpf) = '0';                                                              
              Open Vttbcpf;                                                                         
             Endif;                                                                                 
                                                                                                    
             If %Open (Vttbcsf) = '0';                                                              
              Open Vttbcsf;                                                                         
             Endif;                                                                                 
                                                                                                    
            Dow Pbnc = *Zeros And *In03 = *Off And *In12 = *Off And                                 
                Pdbnc = *Blanks;                                                                    
                Pmensaje = 'Debe Seleccionar el Banco.';                                            
                *In30 = *On;                                                                        
                Exfmt Rdato5;                                                                       
                *In30 = *Off;                                                                       
                Pmensaje = *Blanks;                                                                 
                Fsw = *Blanks;                                                                      
            Enddo;                                                                                  
            If Pbnc <> *Zeros And *In12 = *Off;                                                     
             Dow Fsw = *Blanks;                                                                     
              Exsr srGenerar;                                                                       
              If Nrr1 <= *Zeros;                                                                    
               Pdbnc = *Blanks;                                                                     
               Pdbnc = 'No Existen Estructuras para este Banco.';                                   
               Exfmt Rdato5;                                                                        
               Leave;                                                                               
              Endif;                                                                                
                *In90 = *On;                                                                        
                Exfmt Rcontrol1;                                                                    
                *In90 = *Off;                                                                       
                                                                                                    
               If *In03 = *Off And *In12 = *Off And Nrr1 > *Zeros;                                  
                Readc Rdatos1;                                                                      
                Dow Not %Eof();                                                                     
                 Select;                                                                            
                 When Pop = '1';                                                                    
                  Fsw = 'S';                                                                        
                  Pdcbn1 = Pcbc;                                                                    
                  Pdbnc = Pdbc;                                                                     
                 Endsl;                                                                             
                 Pop = *Blanks;                                                                     
                 Update Rdatos1;                                                                    
                 Readc Rdatos1;                                                                     
                Enddo;                                                                              
               Endif;                                                                               
             Enddo;                                                                                 
             Pbnc = *Zeros;                                                                         
            Endif;                                                                                  
                                                                                                    
            Dow Pest1 = *Zeros And *In03 = *Off And *In12 = *Off And                                
                Pdestr = *Blanks;                                                                   
                Pmensaje = 'Debe Seleccionar la Estructura.';                                       
                *In31 = *On;                                                                        
                Exfmt Rdato5;                                                                       
                *In31 = *Off;                                                                       
                Pmensaje = *Blanks;                                                                 
                Fsw = *Blanks;                                                                      
            Enddo;                                                                                  
            If Pest1 <> *Zeros And *In12 = *Off;                                                    
              Dow Fsw = *Blanks;                                                                    
              Exsr srEstructuras;                                                                   
              If Nrr2 <= *Zeros;                                                                    
               Pdestr = *Blanks;                                                                    
               Pdestr = 'No Existen Estructuras para este Banco.';                                  
               Exfmt Rdato5;                                                                        
               Leave;                                                                               
              Endif;                                                                                
               *In90 = *On;                                                                         
               Exfmt Rcontrol2;                                                                     
               *In90 = *Off;                                                                        
               If *In03 = *Off And *In12 = *Off And Nrr2 > *Zeros;                                  
                Readc Rdatos2;                                                                      
                Dow Not %Eof();                                                                     
                 Select;                                                                            
                 When Pop = '1';                                                                    
                  Pest1 = *Zeros;                                                                   
                  Fsw = 'S';                                                                        
                  If TipoEstr = 'P';                                                                
                   Pdestr = %Trim(Pdes) + ' Por Posiciones' ;                                       
                  ElseIf TipoEstr = 'S';                                                            
                   Pdestr = %Trim(Pdes) + ' Por Separadores [' +                                    
                            %Trim(Pisep) + ']';                                                     
                  Endif;                                                                            
                  Ptipo = TipoEstr;                                                                 
                 Endsl;                                                                             
                 Pop = *Blanks;                                                                     
                 Update Rdatos2;                                                                    
                 Readc Rdatos2;                                                                     
                Enddo;                                                                              
               Endif;                                                                               
             Enddo;                                                                                 
             Pcbc = *Zeros;                                                                         
            Endif;                                                                                  
                                                                                                    
            Dow Parch = *Zeros And *In03 = *Off And *In12 = *Off And                                
                Pdarch = *Blanks And Nrr2 > *Zeros;                                                 
                Pmensaje = 'Debe Seleccionar el Archivo.';                                          
                *In32 = *On;                                                                        
                Exfmt Rdato5;                                                                       
                *In32 = *Off;                                                                       
                Pmensaje = *Blanks;                                                                 
                Fsw = *Blanks;                                                                      
            Enddo;                                                                                  
            If Parch <> *Zeros And *In12 = *Off;                                                    
              Dow Fsw = *Blanks;                                                                    
               Rfile = *Blanks;                                                                     
               Exsr srArchivo;                                                                      
              Enddo;                                                                                
              Parch = *Zeros;                                                                       
            Endif;                                                                                  
                                                                                                    
            If *In12 = *On;                                                                         
             Clear Rdato5;                                                                          
             Iter;                                                                                  
            Endif;                                                                                  
            Enddo;                                                                                  
                                                                                                    
             If %Open (Vtmbncf) = '1';                                                              
              Close Vtmbncf;                                                                        
             Endif;                                                                                 
                                                                                                    
             If %Open (Vttbcpf) = '1';                                                              
              Close Vttbcpf;                                                                        
             Endif;                                                                                 
                                                                                                    
             If %Open (Vttbcsf) = '1';                                                              
              Close Vttbcsf;                                                                        
             Endif;                                                                                 
                                                                                                    
             If %Open (Vtmcaof) = '1';                                                              
              Close Vtmcaof;                                                                        
             Endif;                                                                                 
                                                                                                    
            *Inlr = *On;                                                                            
                                                                                                    
                                                                                                    
                                                                                                    
          //**************Rutina Seleccionar Archivo ********                                       
            Begsr srArchivo;                                                                        
              Chain(n) (Compania:Aplicacion) Vtmcaof;                                               
                If %Found (Vtmcaof);                                                                
                 If Caoest = ' ';                                                                   
                  Ruta = Caorut;                                                                    
                  Callp PgmRuta(Compania:Ruta:Rfile);                                               
                 Endif;                                                                             
                Endif;                                                                              
                fsw = 'S';                                                                          
                Pdarch = Rfile;                                                                     
            Endsr;                                                                                  
                                                                                                    
          //**************Rutina Consulta Bancos       ******                                       
            Begsr srGenerar;                                                                        
              Exsr srBorrar;                                                                        
                                                                                                    
              Exec Sql                                                                              
               Declare C4 Scroll Cursor For                                                         
               Select Bnccod, Bncdes                                                                
               From Vtmbncf                                                                         
               Order By Bncdes;                                                                     
                                                                                                    
              Exec Sql                                                                              
               Open C4;                                                                             
                                                                                                    
              Exec Sql                                                                              
               Fetch C4 Into :Bnccod, :Bncdes;                                                      
                                                                                                    
              Dow Sqlcod <> 100 And Sqlcod >= *Zeros;                                               
               Pop = *Blanks;                                                                       
               Pcbc = Bnccod;                                                                       
               Pdbc = Bncdes;                                                                       
               Nrr1 += 1;                                                                           
               Write Rdatos1;                                                                       
               Exec Sql                                                                             
               Fetch C4 Into :Bnccod, :Bncdes;                                                      
               Enddo;                                                                               
                                                                                                    
              Exec Sql                                                                              
               Close C4;                                                                            
                                                                                                    
               *In50 = *On;                                                                         
               If Nrr1 > *Zeros;                                                                    
                *In95 = *On;                                                                        
               Else;                                                                                
                *In95 = *Off;                                                                       
               Endif;                                                                               
            Endsr;                                                                                  
                                                                                                    
          //**************Rutina Consulta Estructuras  ******                                       
            Begsr srEstructuras;                                                                    
             If Pcbc <> *Zeros And *In12 = *Off;                                                    
              // Estructuras por Posiciones                                                         
              Exsr srBorrar;                                                                        
             Exec Sql                                                                               
             Declare C1 Cursor For                                                                  
             Select Bcpces, Bcpnom                                                                  
             From Vttbcpf                                                                           
             Where Bcpcia = :Compania And Bcpcba = :Pcbc                                            
             Order By Bcpces;                                                                       
                                                                                                    
             Exec Sql                                                                               
              Open C1;                                                                              
                                                                                                    
             Exec Sql                                                                               
              Fetch C1 Into :Bcpces, :Bcpnom;                                                       
                                                                                                    
             Dow Sqlcod <> 100;                                                                     
              Pop = *Blanks;                                                                        
              Pces = Bcpces;                                                                        
              Pdes = %Trim(Bcpnom);                                                                 
              TipoEstr = 'P';                                                                       
              Nrr2 += 1;                                                                            
            //Posi = Nrr2;                                                                          
              Write Rdatos2;                                                                        
              Exec Sql                                                                              
               Fetch C1 Into :Bcpces, :Bcpnom;                                                      
             Enddo;                                                                                 
                                                                                                    
             Exec Sql                                                                               
              Close C1;                                                                             
                                                                                                    
             // Estructuras por Separadores                                                         
             Exec Sql                                                                               
             Declare C2 Cursor For                                                                  
             Select Bcsces, Bcsnom, Bcssep                                                          
             From Vttbcsf                                                                           
             Where Bcscia = :Compania And Bcscba = :Pcbc                                            
             Order By Bcscba;                                                                       
                                                                                                    
             Exec Sql                                                                               
              Open C2;                                                                              
                                                                                                    
             Exec Sql                                                                               
              Fetch C2 Into :Bcsces, :Bcsnom, :Bcssep;                                              
                                                                                                    
             Dow Sqlcod <> 100;                                                                     
              Nrr2 += 1;                                                                            
              Pop = *Blanks;                                                                        
              Pces = Bcsces;                                                                        
              Pdes = Bcsnom;                                                                        
              Pisep = Bcssep;                                                                       
              TipoEstr = 'S';                                                                       
            //Posi = Nrr2;                                                                          
               Write Rdatos2;                                                                       
             Exec Sql                                                                               
              Fetch C2 Into :Bcsces, :Bcsnom, :Bcssep;                                              
             Enddo;                                                                                 
                                                                                                    
             *in50  = *On;                                                                          
                                                                                                    
             Exec Sql                                                                               
              Close C2;                                                                             
              If Nrr2 > *Zeros;                                                                     
               *In95 = *On;                                                                         
              Else;                                                                                 
               *In95 = *Off;                                                                        
              Endif;                                                                                
                                                                                                    
            Endif;                                                                                  
            Endsr;                                                                                  
                                                                                                    
            // ********* Borrar  SubFile *********                                                  
            Begsr srBorrar;                                                                         
             Pbnc = *Zeros;                                                                         
             Pest1 = *Zeros;                                                                        
             Parch = *Zeros;                                                                        
             Nrr1 = *Zeros;                                                                         
             Nrr2 = *Zeros;                                                                         
             *In65 = *On;                                                                           
              Write Rcontrol1;                                                                      
              Write Rcontrol2;                                                                      
              Write RDato5;                                                                         
             *In65 = *Off;                                                                          
            Endsr;                                                                                  
                                                                                                    
      /END-FREE                                                                                     
                                                                                                    
     **********************************************************************                         
     ** Rutina Inicial del Programa                                       *                         
     **********************************************************************                         
     C     *Inzsr        Begsr                                                                      
     C     *Entry        Plist                                                                      
     C                   Parm                    Compania          3                                
     C                   Endsr                                                                      
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Graba Registro en Archivo de Parametros        //                       
       //------------------------------------------------------------------//                       
     PFilePar          B                                                                            
     DFilePar          Pi                                                                           
      /Free                                                                                         
       Pbccia = Compania;                                                                           
       Xcdbn = %Xlate(' ':'0':Xcdbn);                                                               
       Xcdes = %Xlate(' ':'0':Xcdes);                                                               
       Pbccbn = Xcdbn;                                                                              
       Pbcces = Xcdes;                                                                              
       Pbcdes = %Trim(Pdbnc);                                                                       
       Pbctip = Xtpar;                                                                              
       Chain (Pbccbn:Pbccia:Pbcces) Vtwpbcf;                                                        
       If Not %Found(Vtwpbcf);                                                                      
        Write Regpbc;                                                                               
       Endif;                                                                                       
      /End-Free                                                                                     
     PFilePar          E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Graba Registro en el Archivo de Auditoria de   //                       
       // Bancos Subidos                                                   //                       
       //------------------------------------------------------------------//                       
     PGrabarReg        B                                                                            
     DGrabarReg        Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DWsig             s              2a   Inz(*Blanks)                                             
     DWano             s              2a   Inz(*Blanks)                                             
     DWaÑo             s              4a   Inz(*Blanks)                                             
     DWmes             s              2a   Inz(*Blanks)                                             
     DWdia             s              2a   Inz(*Blanks)                                             
     DCurtime          s               T   Inz(*Sys)                                                
     DKfecha           s             10a   Inz(*Blanks)                                             
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
       Chain (Compania:Pdcbn1:%Trim(Xreg):Kfecha) Vttrbsf;                                          
       If Not %Found(Vttrbsf);                                                                      
        Evalr Wsig = %Char(Sig_sys);                                                                
        Rbscia = Compania;                                                                          
        Rbsfca = WaÑo + '/' + Wmes + '/' + Wdia;                                                    
        Rbshca = %Char(Curtime:*Hms);                                                               
        Rbscba = Pdcbn1;                                                                            
        Rbsnba = Pdbnc;                                                                             
        //Rbsces = Pdcbn1;                                                                          
        Rbsces = Pces;                                                                              
        Rbsnes = Pdestr;                                                                            
        Rbsfar = *Blanks;                                                                           
        Rbstre = *Zeros;                                                                            
        Rbsvto = *Zeros;                                                                            
        Rbsusr = User_Name;                                                                         
        Rbsnar = Xfile;                                                                             
        Exec Sql                                                                                    
         Select Trim(Replace(:Xreg, '!', ' ')) Into :Xreg                                           
         From Sysibm/Sysdummy1;                                                                     
        Rbsrar = %Trim(Xreg);                                                                       
        Rbsinp = 'P';                                                                               
        Monitor;                                                                                    
         Write Regrbs;                                                                              
        On-Error;                                                                                   
        Endmon;                                                                                     
       Endif;                                                                                       
      /End-Free                                                                                     
     PGrabarReg        E                                                                            
