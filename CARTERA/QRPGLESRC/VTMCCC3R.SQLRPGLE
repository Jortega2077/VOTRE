***************************************************************************                         
     **                                                                   *                         
     ** Programa      : VTMCCC3R                                          *                         
     ** Descripciòn:  : Crear Estructura de Bancos Opi                    *                         
     ** Proyecto      : Menú de Opciones Opi Paises                       *                         
     ** Autor         : (Jortega)                                         *                         
     ** Fecha Creaciòn: 06 de Enero de 2009                               *                         
     **                                                                   *                         
     **********************************************************************                         
     ** Definición de Directivas de Compilación                                                     
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
                                                                                                    
     **Definiciòn de Archivos de Trabajo                                                            
     FVtmbncf   If   e           k Disk                                                             
     FVtmffbf   If   e           k Disk                                                             
     FVttbcpf   Uf a e           k Disk                                                             
     FVttbcsf   Uf a e           k Disk                                                             
     FVtwcabf2  If a e           k Disk                                                             
                                                                                                    
     ** Definiciòn de Archivo de Pantallas                                                          
     FVtbnci0p  Cf   e             Workstn                                                          
     F                                     Sfile(Rdatos:Nrr)                                        
     F                                     Sfile(Rdatos1:Nrr1)                                      
     F                                     Sfile(Rdatos2:Nrr1)                                      
                                                                                                    
     ** Definiciòn de Variables de Trabajo                                                          
     DWsw              s              1a   Inz(*Blanks)                                             
     DOsw              s              1a   Inz(*Blanks)                                             
     DPsw              s              1a   Inz(*Blanks)                                             
     DXcod             s              3s 0 Inz(1)                                                   
     DPosix            s              4s 0 Inz(*Zeros)                                              
     DXcia             s              3    Inz(*Blanks)                                             
     DXpais            s             20    Inz(*Blanks)                                             
     DSqlStr           s           1000a   Inz(*Blanks)                                             
     DSqlNr1           s            500a   Inz(*Blanks)                                             
     DSqlNr2           s            500a   Inz(*Blanks)                                             
     DPtr              s               *   Inz(%Addr(*In))                                          
                                                                                                    
     ** Definiciòn de Estructuras de Trabajo                                                        
     D                Sds                                                                           
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
     DQbcpcia                              Like(Bcpcia)                                             
     DQbcpcba                              Like(Bcpcba)                                             
     DQbcpban                              Like(Bcpban)                                             
     DQbcpces                              Like(Bcpces)                                             
     DQbcpnom                              Like(Bcpnom)                                             
     DQbcppir                              Like(Bcppir)                                             
     DQbcppfr                              Like(Bcppfr)                                             
     DQbcpidr                              Like(Bcpidr)                                             
     DQbcppit                              Like(Bcppit)                                             
     DQbcppft                              Like(Bcppft)                                             
     DQbcpidt                              Like(Bcpidt)                                             
     DQbcppiv                              Like(Bcppiv)                                             
     DQbcppfv                              Like(Bcppfv)                                             
     DQbcpnde                              Like(Bcpnde)                                             
     DQbcpsde                              Like(Bcpsde)                                             
     DQbcpcev                              Like(Bcpcev)                                             
     DQbcprev                              Like(Bcprev)                                             
     DQbcppic                              Like(Bcppic)                                             
     DQbcppfc                              Like(Bcppfc)                                             
     DQbcpcec                              Like(Bcpcec)                                             
     DQbcprec                              Like(Bcprec)                                             
     DQbcppif                              Like(Bcppif)                                             
     DQbcppff                              Like(Bcppff)                                             
     DQbcpffe                              Like(Bcpffe)                                             
     DQbcppi1                              Like(Bcppi1)                                             
     DQbcppf1                              Like(Bcppf1)                                             
     DQbcpcc1                              Like(Bcpcc1)                                             
     DQbcpnc1                              Like(Bcpnc1)                                             
     DQbcppi2                              Like(Bcppi2)                                             
     DQbcppf2                              Like(Bcppf2)                                             
     DQbcpnc2                              Like(Bcpnc2)                                             
     DQbcpidc                              Like(Bcpidc)                                             
     DQbcptic                              Like(Bcptic)                                             
     DQbcptfc                              Like(Bcptfc)                                             
     DQbcpndc                              Like(Bcpndc)                                             
     DQbcpsdc                              Like(Bcpsdc)                                             
     DQbcpcrc                              Like(Bcpcrc)                                             
     DQbcprpc                              Like(Bcprpc)                                             
                                                                                                    
     DFields1          Ds                                                                           
     DQbcscia                              Like(Bcscia)                                             
     DQbcscba                              Like(Bcscba)                                             
     DQbcsban                              Like(Bcsban)                                             
     DQbcsces                              Like(Bcsces)                                             
     DQbcsnom                              Like(Bcsnom)                                             
     DQbcssep                              Like(Bcssep)                                             
     DQbcspor                              Like(Bcspor)                                             
     DQbcsidr                              Like(Bcsidr)                                             
     DQbcspot                              Like(Bcspot)                                             
     DQbcsidt                              Like(Bcsidt)                                             
     DQbcspov                              Like(Bcspov)                                             
     DQbcsndv                              Like(Bcsndv)                                             
     DQbcssdv                              Like(Bcssdv)                                             
     DQbcscev                              Like(Bcscev)                                             
     DQbcsrev                              Like(Bcsrev)                                             
     DQbcspoc                              Like(Bcspoc)                                             
     DQbcscec                              Like(Bcscec)                                             
     DQbcsrec                              Like(Bcsrec)                                             
     DQbcspof                              Like(Bcspof)                                             
     DQbcsffe                              Like(Bcsffe)                                             
     DQbcspc1                              Like(Bcspc1)                                             
     DQbcscc1                              Like(Bcscc1)                                             
     DQbcsnc1                              Like(Bcsnc1)                                             
     DQbcspc2                              Like(Bcspc2)                                             
     DQbcsnc2                              Like(Bcsnc2)                                             
     DQbcsidc                              Like(Bcsidc)                                             
     DQbcsptc                              Like(Bcsptc)                                             
     DQbcsndc                              Like(Bcsndc)                                             
     DQbcssdc                              Like(Bcssdc)                                             
     DQbcscrc                              Like(Bcscrc)                                             
     DQbcsrpc                              Like(Bcsrpc)                                             
                                                                                                    
     ** Definiciòn de Prototipos de Trabajo                                                         
     DLlenarSfl        Pr                                                                           
     DBorraSfl         Pr                                                                           
     DPintarSfl        Pr                                                                           
     DCrearNov         Pr                                                                           
     DLlenarSflBn      Pr                                                                           
     DBorraSflBn       Pr                                                                           
     DPintarSflBn      Pr                                                                           
     DValidarCre       Pr                                                                           
     DCaptuFieldsPs    Pr                                                                           
     DCaptuFieldsSp    Pr                                                                           
     DLlenarSflFc      Pr                                                                           
     DBorraSflFc       Pr                                                                           
     DPintarSflFc      Pr                                                                           
     DValidarCamp      Pr                                                                           
     DValidarCamp1     Pr                                                                           
     DValidarCamp2     Pr                                                                           
     DBorrarRegFileP   Pr                                                                           
     DBorrarRegFileS   Pr                                                                           
     DCrearRegFileP    Pr                                                                           
     DCrearRegFileS    Pr                                                                           
     DConsecutivo      Pr                                                                           
     DValidarEst       Pr                                                                           
     DConsuRegFileP    Pr                                                                           
     DConsuRegFileS    Pr                                                                           
     DModifiRegFileP   Pr                                                                           
     DModifiRegFileS   Pr                                                                           
     DBuscarRegFileP   Pr                                                                           
     DBuscarRegFileS   Pr                                                                           
                                                                                                    
     DPgmPais          Pr                  Extpgm('VOTREP00/VTMCSBBR')                              
     DXcia                            3                                                             
     DXpais                          20                                                             
                                                                                                    
     DMain             Pr                  Extpgm('Main')                                           
     DPcia                            3a                                                            
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
     DPcia                            3a                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       //              P r o g r a m a   P r i n c i p a l                 //                       
       //------------------------------------------------------------------//                       
      /Free                                                                                         
         PgmPais(Pcia:Xpais);                                                                       
         Dow Salir = *Off;                                                                          
          Pfecha = %Char(%Date());                                                                  
          PintarSfl();                                                                              
          If Pbnc <> *Zeros And Salir = *Off And Cancelar = *Off                                    
           And Crear = *Off;                                                                        
           PintarSflBn();                                                                           
          ElseIf Pdbnc = *Blanks And Salir = *Off And Cancelar = *Off;                              
           *In49 = *On;                                                                             
          Endif;                                                                                    
          If Salir = *On;                                                                           
           Leave;                                                                                   
          Endif;                                                                                    
          If Cancelar = *On;                                                                        
           Clear Rcontrol;                                                                          
           Iter;                                                                                    
          Endif;                                                                                    
         Enddo;                                                                                     
        *Inlr = *On;                                                                                
      /End-Free                                                                                     
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Llena el Sub Archivo del Sub File              //                       
       //------------------------------------------------------------------//                       
     PLlenarSfl        B                                                                            
     DLlenarSfl        Pi                                                                           
      /Free                                                                                         
       BorraSfl();                                                                                  
       // Consulta Información por Banco y sus Respectivas Estructuras                              
       // Estructuras por Posiciones                                                                
       Exec Sql                                                                                     
        Declare C1 Cursor For                                                                       
        Select *                                                                                    
        From Vttbcpf                                                                                
        Where Bcpcia = :Pcia And Bcpcba = :Pcbnc                                                    
        Order By Bcpcba, Bcpban;                                                                    
                                                                                                    
       Exec Sql                                                                                     
        Open C1;                                                                                    
                                                                                                    
       Exec Sql                                                                                     
        Fetch C1 Into :Fields;                                                                      
                                                                                                    
        Dow Sqlcod <> 100;                                                                          
         Write Rdato1;                                                                              
         Pop = *Blanks;                                                                             
         Pcbnc1 = Qbcpcba;                                                                          
         Pdest = Qbcpnom;                                                                           
         Ptipo = 'Por Posiciones';                                                                  
         Pcdes = Qbcpces;                                                                           
         Nrr += 1;                                                                                  
         Posi = Nrr;                                                                                
         Write Rdatos;                                                                              
         Exec Sql                                                                                   
         Fetch C1 Into :Fields;                                                                     
        Enddo;                                                                                      
                                                                                                    
       Exec Sql                                                                                     
        Close C1;                                                                                   
                                                                                                    
       // Consulta Información por Banco y sus Respectivas Estructuras                              
       // Estructuras por Separadores                                                               
       Exec Sql                                                                                     
        Declare C2 Cursor For                                                                       
        Select *                                                                                    
        From Vttbcsf                                                                                
        Where Bcscia = :Pcia And Bcscba = :Pcbnc                                                    
        Order By Bcscba, Bcsban;                                                                    
                                                                                                    
       Exec Sql                                                                                     
        Open C2;                                                                                    
                                                                                                    
       Exec Sql                                                                                     
        Fetch C2 Into :Fields1;                                                                     
                                                                                                    
        Dow Sqlcod <> 100;                                                                          
         Write Rdato1;                                                                              
         Nrr += 1;                                                                                  
         Pop = *Blanks;                                                                             
         Pcbnc1 = Qbcscba;                                                                          
         Pdest = Qbcsnom;                                                                           
         Ptipo = 'Por Separadores';                                                                 
         Pcdes = Qbcsces;                                                                           
         Posi = Nrr;                                                                                
         Write Rdatos;                                                                              
         Exec Sql                                                                                   
         Fetch C2 Into :Fields1;                                                                    
        Enddo;                                                                                      
        SflEnd = *On;                                                                               
                                                                                                    
       Exec Sql                                                                                     
        Close C2;                                                                                   
                                                                                                    
        SflEnd = *On;                                                                               
        If Nrr > *Zeros;                                                                            
         SflDsp = *On;                                                                              
        Else;                                                                                       
         SflDsp = *Off;                                                                             
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
       If Pcbnc <> *Zeros And Salir = *Off And Cancelar = *Off;                                     
        LlenarSfl();                                                                                
       Endif;                                                                                       
       If Posix > *Zeros;                                                                           
        Posic = Posix;                                                                              
       Endif;                                                                                       
       Ppais = Xpais;                                                                               
       SflDspCtl = *On;                                                                             
       Write Rdato8;                                                                                
       Write Rteclas;                                                                               
       Exfmt Rcontrol;                                                                              
       Msg4 = *Off;                                                                                 
       SflDspCtl = *Off;                                                                            
       *In49 = *Off;                                                                                
       If Crear = *On;                                                                              
        CrearNov();                                                                                 
       Endif;                                                                                       
       If *In09 = *On;                                                                              
       Endif;                                                                                       
       If Salir = *Off And Cancelar = *Off And Nrr > *Zeros;                                        
        Readc Rdatos;                                                                               
         Dow Not %Eof();                                                                            
          Select;                                                                                   
           When Pop = '2';                                                                          
            Posix = Posi;                                                                           
            If %Trim(Ptipo) = 'Por Posiciones';                                                     
             ModifiRegFileP();                                                                      
            ElseIf %Trim(Ptipo) = 'Por Separadores';                                                
             ModifiRegFileS();                                                                      
            Endif;                                                                                  
           When Pop = '4';                                                                          
            Posix = 1;                                                                              
            If %Trim(Ptipo) = 'Por Posiciones';                                                     
             BorrarRegFileP();                                                                      
            ElseIf %Trim(Ptipo) = 'Por Separadores';                                                
             BorrarRegFileS();                                                                      
            Endif;                                                                                  
           When Pop = '5';                                                                          
            Posix = Posi;                                                                           
            If %Trim(Ptipo) = 'Por Posiciones';                                                     
             ConsuRegFileP();                                                                       
            ElseIf %Trim(Ptipo) = 'Por Separadores';                                                
             ConsuRegFileS();                                                                       
            Endif;                                                                                  
          Endsl;                                                                                    
          Pop = *Blanks;                                                                            
          Update Rdatos;                                                                            
         Readc Rdatos;                                                                              
        Enddo;                                                                                      
        Endif;                                                                                      
      /End-Free                                                                                     
     PPintarSfl        E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Crea Registros para Alterar la Cartera         //                       
       //------------------------------------------------------------------//                       
     PCrearNov         B                                                                            
     DCrearNov         Pi                                                                           
      /Free                                                                                         
       Clear Rdato2;                                                                                
       Dow Salir = *Off;                                                                            
        Pdbnc1 = Pdbnc;                                                                             
        *In96 = *On;                                                                                
        Write Rdato8;                                                                               
        Exfmt Rdato2;                                                                               
        *In96 = *Off;                                                                               
        *In30 = *Off;                                                                               
        If Salir = *On;                                                                             
         Leave;                                                                                     
        Endif;                                                                                      
        If Cancelar = *On;                                                                          
         Clear Rdato2;                                                                              
         Iter;                                                                                      
        Endif;                                                                                      
        ValidarCre();                                                                               
        If Salir = *Off And Cancelar = *Off;                                                        
         If Pcest1 = 1;                                                                             
          CaptuFieldsPs();                                                                          
         ElseIf Pcest1 = 2;                                                                         
          CaptuFieldsSp();                                                                          
         Endif;                                                                                     
        Endif;                                                                                      
        If Cancelar = *On;                                                                          
         Ptips = *Blanks;                                                                           
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
       Exec Sql                                                                                     
        Declare C3 Scroll Cursor For                                                                
        Select Bnccod, Bncdes                                                                       
        From Vtmbncf                                                                                
        Order By Bncdes;                                                                            
                                                                                                    
       Exec Sql                                                                                     
        Open C3;                                                                                    
                                                                                                    
       Exec Sql                                                                                     
        Fetch C3 Into :Bnccod, :Bncdes;                                                             
                                                                                                    
        Dow Sqlcod <> 100 And Sqlcod >= *Zeros;                                                     
         Pop = *Blanks;                                                                             
         Pcbc = Bnccod;                                                                             
         Pdbc = Bncdes;                                                                             
         Nrr1 += 1;                                                                                 
         Write Rdatos1;                                                                             
       Exec Sql                                                                                     
        Fetch C3 Into :Bnccod, :Bncdes;                                                             
        Enddo;                                                                                      
                                                                                                    
       Exec Sql                                                                                     
        Close C3;                                                                                   
                                                                                                    
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
            Pbnc = *Zeros;                                                                          
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
       // Procedimiento que Valida la Creación de la Estructura            //                       
       //------------------------------------------------------------------//                       
     PValidarCre       B                                                                            
     DValidarCre       Pi                                                                           
      /Free                                                                                         
       Dow Pdest1 = *Blanks And Salir = *Off And Cancelar = *Off;                                   
        *In32 = *On;                                                                                
        Exfmt Rdato2;                                                                               
        *In32 = *Off;                                                                               
       Enddo;                                                                                       
       Dow Pcest1 = *Zeros And Salir = *Off And Cancelar = *Off;                                    
        *In33 = *On;                                                                                
        Exfmt Rdato2;                                                                               
        *In33 = *Off;                                                                               
       Enddo;                                                                                       
       If Pcest1 = 2;                                                                               
        *In30 = *On;                                                                                
       Endif;                                                                                       
       Wsw = 'S';                                                                                   
       Dow (Ptips = *Blanks And Salir = *Off And Cancelar = *Off And Wsw =                          
       'S') Or (Ptips <> *Blanks And Salir = *Off And                                               
       Cancelar = *Off And Wsw = 'S');                                                              
        ValidarEst();                                                                               
        If Wsw <> *Blanks And Salir = *Off And Cancelar = *Off;                                     
         *In78 = *On;                                                                               
         Exfmt Rdato2;                                                                              
         *In78 = *Off;                                                                              
        Endif;                                                                                      
       If Pcest1 = 2;                                                                               
        *In30 = *On;                                                                                
       Endif;                                                                                       
        Dow Ptips = *Blanks And Salir = *Off And Cancelar = *Off And                                
         Pcest1 = 2;                                                                                
         *In34 = *On;                                                                               
         Exfmt Rdato2;                                                                              
         *In34 = *Off;                                                                              
         Wsw = 'S';                                                                                 
        Enddo;                                                                                      
       Enddo;                                                                                       
      /End-Free                                                                                     
     PValidarCre       E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Captura los Campos que se Necesitaran de la    //                       
       // Estructura del Archivo del Banco por Posiciones                  //                       
       //------------------------------------------------------------------//                       
     PCaptuFieldsPs    B                                                                            
     DCaptuFieldsPs    Pi                                                                           
      /Free                                                                                         
       Clear Rdato4;                                                                                
       Clear Rdato5;                                                                                
       Dow Salir = *Off;                                                                            
        Pdbncp = Pdbnc1;                                                                            
        Pestp = Pdest1;                                                                             
        Pcomp = 2;                                                                                  
        *In28 = *On;                                                                                
        *In51 = *On;                                                                                
        Exfmt Rdato4;                                                                               
        *In51 = *Off;                                                                               
        ValidarCamp();                                                                              
        If Pforp <> *Zeros And Salir = *Off And Cancelar = *Off And                                 
        *In10 = *Off;                                                                               
         PintarSflFc();                                                                             
         Iter;                                                                                      
        Endif;                                                                                      
        Dow *In10 = *On And Salir = *Off And Cancelar = *Off;                                       
        *In28 = *On;                                                                                
        Exfmt Rdato5;                                                                               
        ValidarCamp1();                                                                             
        // Crea Registro en el Archivo de Posiciones                                                
        If *In08 = *On;                                                                             
         Psw = 'I';                                                                                 
         CrearRegFileP();                                                                           
        Endif;                                                                                      
        If Cancelar = *On;                                                                          
         Cancelar = *Off;                                                                           
         Leave;                                                                                     
        Else;                                                                                       
         *In10 = *On;                                                                               
        Endif;                                                                                      
        Enddo;                                                                                      
        *In10 = *Off;                                                                               
        If Salir = *On;                                                                             
         Leave;                                                                                     
        Endif;                                                                                      
        // Crea Registro en el Archivo de Posiciones                                                
        If *In08 = *On;                                                                             
         Psw = 'I';                                                                                 
         CrearRegFileP();                                                                           
        Endif;                                                                                      
        If Cancelar = *On;                                                                          
         Clear Rdato4;                                                                              
         Clear Rdato5;                                                                              
         Iter;                                                                                      
        Endif;                                                                                      
       Enddo;                                                                                       
       If Salir = *On And *In08 = *Off;                                                             
        Salir = *Off;                                                                               
       Endif;                                                                                       
      /End-Free                                                                                     
     PCaptuFieldsPs    E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Captura los Campos que se Necesitaran de la    //                       
       // Estructura del Archivo del Banco por Separadores                 //                       
       //------------------------------------------------------------------//                       
     PCaptuFieldsSp    B                                                                            
     DCaptuFieldsSp    Pi                                                                           
      /Free                                                                                         
       Clear Rdato6;                                                                                
       Clear Rdato7;                                                                                
       Dow Salir = *Off;                                                                            
        Pdbncp = Pdbnc1;                                                                            
        Pestp = Pdest1;                                                                             
        Psepp = Ptips;                                                                              
        Pcomp = 2;                                                                                  
        *In28 = *On;                                                                                
        Exfmt Rdato6;                                                                               
        ValidarCamp2();                                                                             
        If Pforp <> *Zeros And Salir = *Off And Cancelar = *Off And                                 
        *In10 = *Off;                                                                               
         PintarSflFc();                                                                             
         Iter;                                                                                      
        Endif;                                                                                      
        Dow *In10 = *On And Salir = *Off And Cancelar = *Off;                                       
         *In28 = *On;                                                                               
         Exfmt Rdato7;                                                                              
        // Crea Registro en el Archivo de Separadores                                               
        If *In08 = *On;                                                                             
         Psw = 'I';                                                                                 
         CrearRegFileS();                                                                           
        Endif;                                                                                      
        If Cancelar = *On;                                                                          
         Cancelar = *Off;                                                                           
         Leave;                                                                                     
        Else;                                                                                       
         *In10 = *On;                                                                               
        Endif;                                                                                      
        Enddo;                                                                                      
        *In10 = *Off;                                                                               
        If Salir = *On;                                                                             
         Leave;                                                                                     
        Endif;                                                                                      
        // Crea Registro en el Archivo de Separadores                                               
        If *In08 = *On;                                                                             
         Psw = 'I';                                                                                 
         CrearRegFileS();                                                                           
        Endif;                                                                                      
        If Cancelar = *On;                                                                          
         Clear Rdato6;                                                                              
         Clear Rdato7;                                                                              
         Iter;                                                                                      
        Endif;                                                                                      
       Enddo;                                                                                       
       If Salir = *On And *In08 = *Off;                                                             
        Salir = *Off;                                                                               
       Endif;                                                                                       
      /End-Free                                                                                     
     PCaptuFieldsSp    E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Llena el Sub Archivo del Sub File              //                       
       //------------------------------------------------------------------//                       
     PLlenarSflFc      B                                                                            
     DLlenarSflFc      Pi                                                                           
      /Free                                                                                         
       BorraSflFc();                                                                                
       Setll *Loval Vtmffbf;                                                                        
        Read Vtmffbf;                                                                               
        Dow Not %Eof(Vtmffbf);                                                                      
         Pop = *Blanks;                                                                             
         Pcfec = Ffbcod;                                                                            
         Pdfec = Ffbdes;                                                                            
         Nrr1 += 1;                                                                                 
         Write Rdatos2;                                                                             
        Read Vtmffbf;                                                                               
        Enddo;                                                                                      
        *In50 = *On;                                                                                
        If Nrr1 > *Zeros;                                                                           
         *In95 = *On;                                                                               
        Else;                                                                                       
         *In95 = *Off;                                                                              
        Endif;                                                                                      
      /End-Free                                                                                     
     PLlenarSflFc      E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Borra el Sub Archivo de la Pantalla            //                       
       //------------------------------------------------------------------//                       
     PBorraSflFc       B                                                                            
     DBorraSflFc       Pi                                                                           
      /Free                                                                                         
       Nrr1 = *Zeros;                                                                               
       *In65 = *On;                                                                                 
       Write Rcontrol2;                                                                             
       *In65 = *Off;                                                                                
      /End-Free                                                                                     
     PBorraSflFc       E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Pinta el Sub Archivo de la Pantalla            //                       
       //------------------------------------------------------------------//                       
     PPintarSflFc      B                                                                            
     DPintarSflFc      Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DFsw              s              1a   Inz(*Blanks)                                             
      /Free                                                                                         
       Dow Fsw = *Blanks;                                                                           
       LlenarSflFc();                                                                               
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
            Pdforp = Pdfec;                                                                         
            Pforp = *Zeros;                                                                         
          Endsl;                                                                                    
          Pop = *Blanks;                                                                            
          Update Rdatos2;                                                                           
         Readc Rdatos2;                                                                             
        Enddo;                                                                                      
        Endif;                                                                                      
       Enddo;                                                                                       
      /End-Free                                                                                     
     PPintarSflFc      E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Valida la Creación de la Estructura por Posici //                       
       //------------------------------------------------------------------//                       
     PValidarCamp      B                                                                            
     DValidarCamp      Pi                                                                           
      /Free                                                                                         
       // Validaciones Campo Tipo de Registro Archivo                                               
       Dow Ptrip <> *Zeros And Ptrfp = *Zeros                                                       
        And Salir = *Off And Cancelar = *Off;                                                       
        *In53 = *On;                                                                                
        *In60 = *On;                                                                                
        Exfmt Rdato4;                                                                               
        *In53 = *Off;                                                                               
        *In60 = *Off;                                                                               
       Enddo;                                                                                       
       Dow Ptrfp <> *Zeros And Ptrip = *Zeros                                                       
        And Salir = *Off And Cancelar = *Off;                                                       
        *In52 = *On;                                                                                
        *In59 = *On;                                                                                
        Exfmt Rdato4;                                                                               
        *In52 = *Off;                                                                               
        *In59 = *Off;                                                                               
       Enddo;                                                                                       
       Dow Ptrip <> *Zeros And Ptrfp <> *Zeros And Ptrfp < Ptrip                                    
       And Salir = *Off And Cancelar = *Off;                                                        
        *In54 = *On;                                                                                
        *In59 = *On;                                                                                
        Exfmt Rdato4;                                                                               
        *In54 = *Off;                                                                               
        *In59 = *Off;                                                                               
       Enddo;                                                                                       
       // Validaciones Campo Tipo Transacción                                                       
       Dow Ptprip > *Zeros And Ptprfp = *Zeros                                                      
        And Salir = *Off And Cancelar = *Off;                                                       
        *In53 = *On;                                                                                
        *In62 = *On;                                                                                
        Exfmt Rdato4;                                                                               
        *In53 = *Off;                                                                               
        *In62 = *Off;                                                                               
       Enddo;                                                                                       
       Dow Ptprfp <> *Zeros And Ptprip = *Zeros                                                     
        And Salir = *Off And Cancelar = *Off;                                                       
        *In52 = *On;                                                                                
        *In61 = *On;                                                                                
        Exfmt Rdato4;                                                                               
        *In52 = *Off;                                                                               
        *In61 = *Off;                                                                               
       Enddo;                                                                                       
       Dow Ptprip <> *Zeros And Ptprfp <> *Zeros And Ptprfp < Ptprip                                
       And Salir = *Off And Cancelar = *Off;                                                        
        *In54 = *On;                                                                                
        *In61 = *On;                                                                                
        Exfmt Rdato4;                                                                               
        *In54 = *Off;                                                                               
        *In61 = *Off;                                                                               
       Enddo;                                                                                       
       // Validaciones Campo Valor                                                                  
       Dow Pposip = *Zeros And Salir = *Off And Cancelar = *Off;                                    
        *In52 = *On;                                                                                
        *In55 = *On;                                                                                
        Exfmt Rdato4;                                                                               
        *In52 = *Off;                                                                               
        *In55 = *Off;                                                                               
       Enddo;                                                                                       
       Dow Pposfp = *Zeros And Salir = *Off And Cancelar = *Off;                                    
        *In53 = *On;                                                                                
        *In56 = *On;                                                                                
        Exfmt Rdato4;                                                                               
        *In53 = *Off;                                                                               
        *In56 = *Off;                                                                               
       Enddo;                                                                                       
       Dow Pposip <> *Zeros And Pposfp <> *Zeros And Pposfp < Pposip                                
       And Salir = *Off And Cancelar = *Off;                                                        
        *In54 = *On;                                                                                
        *In55 = *On;                                                                                
        Exfmt Rdato4;                                                                               
        *In54 = *Off;                                                                               
        *In55 = *Off;                                                                               
       Enddo;                                                                                       
       // Validaciones Campo Cédula                                                                 
       If Pcia <> '100' And Pcia <> '600';                                                          
       Dow Pcdip = *Zeros And Salir = *Off And Cancelar = *Off;                                     
        *In52 = *On;                                                                                
        *In63 = *On;                                                                                
        Exfmt Rdato4;                                                                               
        *In52 = *Off;                                                                               
        *In63 = *Off;                                                                               
       Enddo;                                                                                       
       Dow Pcdfp = *Zeros And Salir = *Off And Cancelar = *Off;                                     
        *In53 = *On;                                                                                
        *In64 = *On;                                                                                
        Exfmt Rdato4;                                                                               
        *In53 = *Off;                                                                               
        *In64 = *Off;                                                                               
       Enddo;                                                                                       
       Endif;                                                                                       
       // Validaciones Campo Cédula                                                                 
       If Pcia <> '100' And Pcia <> '600';                                                          
       Dow Pcdip > *Zeros And Pcdfp = *Zeros                                                        
        And Salir = *Off And Cancelar = *Off;                                                       
        *In53 = *On;                                                                                
        *In64 = *On;                                                                                
        Exfmt Rdato4;                                                                               
        *In53 = *Off;                                                                               
        *In64 = *Off;                                                                               
       Enddo;                                                                                       
       If Pcia <> '100' And Pcia <> '600';                                                          
       Dow Pcdfp <> *Zeros And Pcdip = *Zeros                                                       
        And Salir = *Off And Cancelar = *Off;                                                       
        *In52 = *On;                                                                                
        *In63 = *On;                                                                                
        Exfmt Rdato4;                                                                               
        *In52 = *Off;                                                                               
        *In63 = *Off;                                                                               
       Enddo;                                                                                       
       Dow Pcdip <> *Zeros And Pcdfp <> *Zeros And Pcdfp < Pcdip                                    
       And Salir = *Off And Cancelar = *Off;                                                        
        *In54 = *On;                                                                                
        *In63 = *On;                                                                                
        Exfmt Rdato4;                                                                               
        *In54 = *Off;                                                                               
        *In63 = *Off;                                                                               
       Enddo;                                                                                       
       Endif;                                                                                       
       Endif;                                                                                       
       // Validaciones Campo Fecha de Transacción                                                   
       Dow Pfip = *Zeros And Salir = *Off And Cancelar = *Off;                                      
        *In52 = *On;                                                                                
        *In57 = *On;                                                                                
        Exfmt Rdato4;                                                                               
        *In52 = *Off;                                                                               
        *In57 = *Off;                                                                               
       Enddo;                                                                                       
       Dow Pffp = *Zeros And Salir = *Off And Cancelar = *Off;                                      
        *In53 = *On;                                                                                
        *In58 = *On;                                                                                
        Exfmt Rdato4;                                                                               
        *In53 = *Off;                                                                               
        *In58 = *Off;                                                                               
       Enddo;                                                                                       
       Dow Pfip <> *Zeros And Pffp <> *Zeros And Pffp < Pfip                                        
       And Salir = *Off And Cancelar = *Off;                                                        
        *In54 = *On;                                                                                
        *In57 = *On;                                                                                
        Exfmt Rdato4;                                                                               
        *In54 = *Off;                                                                               
        *In57 = *Off;                                                                               
       Enddo;                                                                                       
       Dow Pforp = *Zeros And Salir = *Off And Cancelar = *Off And                                  
       Pdforp = *Blanks;                                                                            
        *In76 = *On;                                                                                
        *In77 = *On;                                                                                
        Exfmt Rdato4;                                                                               
        *In76 = *Off;                                                                               
        *In77 = *Off;                                                                               
       Enddo;                                                                                       
       // Validaciones Campo Adicional 1                                                            
       Dow Pca1if > *Zeros And Pca1ff = *Zeros                                                      
        And Salir = *Off And Cancelar = *Off;                                                       
        *In53 = *On;                                                                                
        *In67 = *On;                                                                                
        Exfmt Rdato4;                                                                               
        *In53 = *Off;                                                                               
        *In67 = *Off;                                                                               
       Enddo;                                                                                       
       Dow Pca1ff <> *Zeros And Pca1if = *Zeros                                                     
        And Salir = *Off And Cancelar = *Off;                                                       
        *In52 = *On;                                                                                
        *In66 = *On;                                                                                
        Exfmt Rdato4;                                                                               
        *In52 = *Off;                                                                               
        *In66 = *Off;                                                                               
       Enddo;                                                                                       
       Dow Pca1if <> *Zeros And Pca1ff <> *Zeros And Pca1ff < Pca1if                                
       And Salir = *Off And Cancelar = *Off;                                                        
        *In54 = *On;                                                                                
        *In66 = *On;                                                                                
        Exfmt Rdato4;                                                                               
        *In54 = *Off;                                                                               
        *In66 = *Off;                                                                               
       Enddo;                                                                                       
       // Validaciones Campo Adicional 2                                                            
       Dow Pca2if > *Zeros And Pca2ff = *Zeros                                                      
        And Salir = *Off And Cancelar = *Off;                                                       
        *In53 = *On;                                                                                
        *In69 = *On;                                                                                
        Exfmt Rdato4;                                                                               
        *In53 = *Off;                                                                               
        *In69 = *Off;                                                                               
       Enddo;                                                                                       
       Dow Pca2ff <> *Zeros And Pca2if = *Zeros                                                     
        And Salir = *Off And Cancelar = *Off;                                                       
        *In52 = *On;                                                                                
        *In68 = *On;                                                                                
        Exfmt Rdato4;                                                                               
        *In52 = *Off;                                                                               
        *In68 = *Off;                                                                               
       Enddo;                                                                                       
       Dow Pca2if <> *Zeros And Pca2ff <> *Zeros And Pca2ff < Pca2if                                
       And Salir = *Off And Cancelar = *Off;                                                        
        *In54 = *On;                                                                                
        *In68 = *On;                                                                                
        Exfmt Rdato4;                                                                               
        *In54 = *Off;                                                                               
        *In68 = *Off;                                                                               
       Enddo;                                                                                       
      /End-Free                                                                                     
     PValidarCamp      E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Valida la Creación de la Estructura por Posici //                       
       //------------------------------------------------------------------//                       
     PValidarCamp1     B                                                                            
     DValidarCamp1     Pi                                                                           
      /Free                                                                                         
       // Validaciones Campo Registro de Chequeo                                                    
       Dow Ptpi > *Zeros And Ptpf = *Zeros                                                          
        And Salir = *Off And Cancelar = *Off;                                                       
        *In53 = *On;                                                                                
        *In73 = *On;                                                                                
        Exfmt Rdato5;                                                                               
        *In53 = *Off;                                                                               
        *In73 = *Off;                                                                               
       Enddo;                                                                                       
       Dow Ptpf <> *Zeros And Ptpi = *Zeros                                                         
        And Salir = *Off And Cancelar = *Off;                                                       
        *In52 = *On;                                                                                
        *In72 = *On;                                                                                
        Exfmt Rdato5;                                                                               
        *In52 = *Off;                                                                               
        *In72 = *Off;                                                                               
       Enddo;                                                                                       
       Dow Ptpi <> *Zeros And Ptpf <> *Zeros And Ptpf < Ptpi                                        
       And Salir = *Off And Cancelar = *Off;                                                        
        *In54 = *On;                                                                                
        *In72 = *On;                                                                                
        Exfmt Rdato5;                                                                               
        *In54 = *Off;                                                                               
        *In72 = *Off;                                                                               
       Enddo;                                                                                       
       Dow Pforp = *Zeros And Salir = *Off And Cancelar = *Off And                                  
       Pdforp = *Blanks;                                                                            
        *In76 = *On;                                                                                
        *In77 = *On;                                                                                
        Exfmt Rdato6;                                                                               
        *In76 = *Off;                                                                               
        *In77 = *Off;                                                                               
       Enddo;                                                                                       
      /End-Free                                                                                     
     PValidarCamp1     E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Valida la Creación de la Estructura por Separa //                       
       //------------------------------------------------------------------//                       
     PValidarCamp2     B                                                                            
     DValidarCamp2     Pi                                                                           
      /Free                                                                                         
       // Validaciones Campo Valor                                                                  
       Dow Pposip = *Zeros And Salir = *Off And Cancelar = *Off;                                    
        *In52 = *On;                                                                                
        *In55 = *On;                                                                                
        Exfmt Rdato6;                                                                               
        *In52 = *Off;                                                                               
        *In55 = *Off;                                                                               
       Enddo;                                                                                       
       // Validaciones Campo Cédula                                                                 
       If Pcia <> '100' And Pcia <> '600';                                                          
       Dow Pcdip = *Zeros And Salir = *Off And Cancelar = *Off;                                     
        *In52 = *On;                                                                                
        *In98 = *On;                                                                                
        Exfmt Rdato6;                                                                               
        *In52 = *Off;                                                                               
        *In98 = *Off;                                                                               
       Enddo;                                                                                       
       Endif;                                                                                       
       // Validaciones Campo Fecha de Transacción                                                   
       Dow Pfip = *Zeros And Salir = *Off And Cancelar = *Off;                                      
        *In52 = *On;                                                                                
        *In57 = *On;                                                                                
        Exfmt Rdato6;                                                                               
        *In52 = *Off;                                                                               
        *In57 = *Off;                                                                               
       Enddo;                                                                                       
       Dow Pforp = *Zeros And Salir = *Off And Cancelar = *Off And                                  
       Pdforp = *Blanks;                                                                            
        *In76 = *On;                                                                                
        *In77 = *On;                                                                                
        Exfmt Rdato6;                                                                               
        *In76 = *Off;                                                                               
        *In77 = *Off;                                                                               
       Enddo;                                                                                       
      /End-Free                                                                                     
     PValidarCamp2     E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Borra Registro del Sub File y del Archivo      //                       
       //------------------------------------------------------------------//                       
     PBorrarRegFileP   B                                                                            
     DBorrarRegFileP   Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
      /Free                                                                                         
       Clear Rdato3;                                                                                
       Dow Popc = *Blanks;                                                                          
        Exfmt Rdato3;                                                                               
        If Popc = 's' Or Popc = 'S';                                                                
         Exec Sql                                                                                   
          Delete From Votrea00/Vttbcpf Where Bcpcia = :Pcia And                                     
          Bcpcba = :Pcbnc And Bcpces = :Pcdes;                                                      
          *In39 = *On;                                                                              
          Exfmt Rdato3;                                                                             
          *In39 = *Off;                                                                             
        Endif;                                                                                      
       Enddo;                                                                                       
      /End-Free                                                                                     
     PBorrarRegFileP   E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Borra Registro del Sub File y del Archivo      //                       
       //------------------------------------------------------------------//                       
     PBorrarRegFileS   B                                                                            
     DBorrarRegFileS   Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
      /Free                                                                                         
       Clear Rdato3;                                                                                
       Dow Popc = *Blanks;                                                                          
        Exfmt Rdato3;                                                                               
        If Popc = 's' Or Popc = 'S';                                                                
         Exec Sql                                                                                   
          Delete From Votrea00/Vttbcsf Where Bcscia = :Pcia And                                     
          Bcscba = :Pcbnc And Bcsces = :Pcdes;                                                      
          *In39 = *On;                                                                              
          Exfmt Rdato3;                                                                             
          *In39 = *Off;                                                                             
        Endif;                                                                                      
       Enddo;                                                                                       
      /End-Free                                                                                     
     PBorrarRegFileS   E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Crea Registro en el Archivo                   //                        
       //------------------------------------------------------------------//                       
     PConsecutivo      B                                                                            
     DConsecutivo      Pi                                                                           
      /Free                                                                                         
       // Incremento por Posiciones                                                                 
       If Osw = 'P';                                                                                
       Exec Sql                                                                                     
        Select Max(Bcpces) Into :Xcod                                                               
        From Vttbcpf                                                                                
        Where Bcpcia = :Pcia And Bcpcba = :Pcbnc;                                                   
                                                                                                    
        If Sqlcod <> 100;                                                                           
         Xcod += 1;                                                                                 
        Endif;                                                                                      
       Endif;                                                                                       
       // Incremento por Separadores                                                                
       If Osw = 'S';                                                                                
       Exec Sql                                                                                     
        Select Max(Bcsces) Into :Xcod                                                               
        From Vttbcsf                                                                                
        Where Bcscia = :Pcia And Bcscba = :Pcbnc;                                                   
                                                                                                    
        If Sqlcod <> 100;                                                                           
         Xcod += 1;                                                                                 
        Endif;                                                                                      
       Endif;                                                                                       
      /End-Free                                                                                     
     PConsecutivo      E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Valida es Estructura del Banco                //                        
       //------------------------------------------------------------------//                       
     PValidarEst       B                                                                            
     DValidarEst       Pi                                                                           
      /Free                                                                                         
       // Validación por Posiciones                                                                 
       If Pcest1 = 1;                                                                               
       Exec Sql                                                                                     
        Select Bcpces Into :Xcod                                                                    
        From Vttbcpf                                                                                
        Where Bcpcia = :Pcia And Bcpcba = :Pcbnc And Bcpnom = :Pdest1;                              
                                                                                                    
        If Sqlcod <> 100;                                                                           
         Wsw = 'S';                                                                                 
        Else;                                                                                       
         Wsw = *Blanks;                                                                             
        Endif;                                                                                      
       Endif;                                                                                       
                                                                                                    
       // Validación por Separadores                                                                
       If Pcest1 = 2;                                                                               
       Exec Sql                                                                                     
        Select Bcsces Into :Xcod                                                                    
        From Vttbcsf                                                                                
        Where Bcscia = :Pcia And Bcscba = :Pcbnc And Bcsnom = :Pdest1;                              
                                                                                                    
        If Sqlcod <> 100;                                                                           
         Wsw = 'S';                                                                                 
        Else;                                                                                       
         Wsw = *Blanks;                                                                             
        Endif;                                                                                      
       Endif;                                                                                       
      /End-Free                                                                                     
     PValidarEst       E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Crea Registro en el Archivo                    //                       
       //------------------------------------------------------------------//                       
     PCrearRegFileP    B                                                                            
     DCrearRegFileP    Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
      /Free                                                                                         
       Salir = *On;                                                                                 
       If Psw = 'I';                                                                                
        Bcpcia = Pcia;                                                                              
        Bcpcba = Pcbnc;                                                                             
        Bcpban = Pdbncp;                                                                            
        Bcpnom = Pestp;                                                                             
        Bcppir = Ptrip;                                                                             
        Bcppfr = Ptrfp;                                                                             
        Bcpidr = Ptride;                                                                            
        Bcppit = Ptprip;                                                                            
        Bcppft = Ptprfp;                                                                            
        Bcpidt = Pttrid;                                                                            
        Bcppiv = Pposip;                                                                            
        Bcppfv = Pposfp;                                                                            
        Bcpnde = Pnrop;                                                                             
        Bcpsde = Pspd;                                                                              
        Bcpcev = Pcrep;                                                                             
        Bcprev = Premp;                                                                             
        Bcppic = Pcdip;                                                                             
        Bcppfc = Pcdfp;                                                                             
        Bcpcec = Pccep;                                                                             
        Bcprec = Pcrmp;                                                                             
        Bcppif = Pfip;                                                                              
        Bcppff = Pffp;                                                                              
        Bcpffe = Pcfec;                                                                             
        Bcppi1 = Pca1if;                                                                            
        Bcppf1 = Pca1ff;                                                                            
        If Pcomp = 01;                                                                              
         Bcpcc1 = 'Si';                                                                             
        Else;                                                                                       
         Bcpcc1 = 'No';                                                                             
       Endif;                                                                                       
        Bcpnc1 = Pnca1p;                                                                            
        Bcppi2 = Pca2if;                                                                            
        Bcppf2 = Pca2ff;                                                                            
        Bcpnc2 = Pnca2p;                                                                            
        Bcpidc = Pidr;                                                                              
        Bcptic = Ptpi;                                                                              
        Bcptfc = Ptpf;                                                                              
        Bcpndc = Pnrdc;                                                                             
        Bcpsdc = Pspdc;                                                                             
        Bcpcrc = Pcrec;                                                                             
        Bcprpc = Prmpc;                                                                             
        Osw = 'P';                                                                                  
        Consecutivo();                                                                              
        Bcpces = Xcod;                                                                              
        Write Regbcp;                                                                               
        *In38 = *On;                                                                                
        Exfmt Rdato1;                                                                               
        *In38 = *Off;                                                                               
       Endif;                                                                                       
                                                                                                    
       If Psw = 'M';                                                                                
       Chain (Pcia:Pcbnc1:Pcdes) Vttbcpf;                                                           
       If %Found(Vttbcpf);                                                                          
        Bcppir = Ptrip;                                                                             
        Bcppfr = Ptrfp;                                                                             
        Bcpidr = Ptride;                                                                            
        Bcppit = Ptprip;                                                                            
        Bcppft = Ptprfp;                                                                            
        Bcpidt = Pttrid;                                                                            
        Bcppiv = Pposip;                                                                            
        Bcppfv = Pposfp;                                                                            
        Bcpnde = Pnrop;                                                                             
        Bcpsde = Pspd;                                                                              
        Bcpcev = Pcrep;                                                                             
        Bcprev = Premp;                                                                             
        Bcppic = Pcdip;                                                                             
        Bcppfc = Pcdfp;                                                                             
        Bcpcec = Pccep;                                                                             
        Bcprec = Pcrmp;                                                                             
        Bcppif = Pfip;                                                                              
        Bcppff = Pffp;                                                                              
        Bcpffe = Pcfec;                                                                             
        Bcppi1 = Pca1if;                                                                            
        Bcppf1 = Pca1ff;                                                                            
        If Pcomp = 01;                                                                              
         Bcpcc1 = 'Si';                                                                             
        Else;                                                                                       
         Bcpcc1 = 'No';                                                                             
       Endif;                                                                                       
        Bcpnc1 = Pnca1p;                                                                            
        Bcppi2 = Pca2if;                                                                            
        Bcppf2 = Pca2ff;                                                                            
        Bcpnc2 = Pnca2p;                                                                            
        Bcpidc = Pidr;                                                                              
        Bcptic = Ptpi;                                                                              
        Bcptfc = Ptpf;                                                                              
        Bcpndc = Pnrdc;                                                                             
        Bcpsdc = Pspdc;                                                                             
        Bcpcrc = Pcrec;                                                                             
        Bcprpc = Prmpc;                                                                             
        Update Regbcp;                                                                              
        *In39 = *On;                                                                                
        Exfmt Rdato1;                                                                               
        *In39 = *Off;                                                                               
        Endif;                                                                                      
       Endif;                                                                                       
      /End-Free                                                                                     
     PCrearRegFileP    E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Crea Registro en el Archivo                    //                       
       //------------------------------------------------------------------//                       
     PCrearRegFileS    B                                                                            
     DCrearRegFileS    Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
      /Free                                                                                         
       Salir = *On;                                                                                 
       If Psw = 'I';                                                                                
        Bcscia = Pcia;                                                                              
        Bcscba = Pcbnc;                                                                             
        Bcsban = Pdbncp;                                                                            
        Bcsnom = Pestp;                                                                             
        Bcssep = Psepp;                                                                             
        Bcspor = Ptrip;                                                                             
        Bcsidr = Ptride;                                                                            
        Bcspot = Ptprip;                                                                            
        Bcsidt = Pttrid;                                                                            
        Bcspov = Pposip;                                                                            
        Bcsndv = Pnrod;                                                                             
        Bcssdv = Pspd;                                                                              
        Bcscev = Pcrep;                                                                             
        Bcsrev = Premp;                                                                             
        Bcspoc = Pcdip;                                                                             
        Bcscec = Pccep;                                                                             
        Bcsrec = Pcrmp;                                                                             
        Bcspof = Pfip;                                                                              
        Bcsffe = Pcfec;                                                                             
        Bcspc1 = Pca1if;                                                                            
         If Pcomp = 01;                                                                             
          Bcscc1 = 'Si';                                                                            
         Else;                                                                                      
          Bcscc1 = 'No';                                                                            
        Endif;                                                                                      
        Bcsnc1 = Pnca1p;                                                                            
        Bcspc2 = Pca2if;                                                                            
        Bcsnc2 = Pnca2p;                                                                            
        Bcsidc = Pidr;                                                                              
        Bcsptc = Ptpi;                                                                              
        Bcsndc = Pnrdc;                                                                             
        Bcssdc = Pspdc;                                                                             
        Bcscrc = Pcrec;                                                                             
        Bcsrpc = Prmpc;                                                                             
        Osw = 'S';                                                                                  
        Consecutivo();                                                                              
        Bcsces = Xcod;                                                                              
        Write Regbcs;                                                                               
        *In38 = *On;                                                                                
        Exfmt Rdato1;                                                                               
        *In38 = *Off;                                                                               
       Endif;                                                                                       
                                                                                                    
       If Psw = 'M';                                                                                
       Chain (Pcia:Pcbnc1:Pcdes) Vttbcsf;                                                           
       If %Found(Vttbcsf);                                                                          
        Bcspor = Ptrip;                                                                             
        Bcsidr = Ptride;                                                                            
        Bcspot = Ptprip;                                                                            
        Bcsidt = Pttrid;                                                                            
        Bcspov = Pposip;                                                                            
        Bcsndv = Pnrod;                                                                             
        Bcssdv = Pspd;                                                                              
        Bcscev = Pcrep;                                                                             
        Bcsrev = Premp;                                                                             
        Bcspoc = Pcdip;                                                                             
        Bcscec = Pccep;                                                                             
        Bcsrec = Premp;                                                                             
        Bcspof = Pfip;                                                                              
        Bcsffe = Pcfec;                                                                             
        Bcspc1 = Pca1if;                                                                            
         If Pcomp = 01;                                                                             
          Bcscc1 = 'Si';                                                                            
         Else;                                                                                      
          Bcscc1 = 'No';                                                                            
        Endif;                                                                                      
        Bcsnc1 = Pnca1p;                                                                            
        Bcspc2 = Pca2if;                                                                            
        Bcsnc2 = Pnca2p;                                                                            
        Bcsidc = Pidr;                                                                              
        Bcsptc = Ptpi;                                                                              
        Bcsndc = Pnrdc;                                                                             
        Bcssdc = Pspdc;                                                                             
        Bcscrc = Pcrec;                                                                             
        Bcsrpc = Prmpc;                                                                             
        Osw = 'S';                                                                                  
        Update Regbcs;                                                                              
        *In39 = *On;                                                                                
        Exfmt Rdato1;                                                                               
        *In39 = *Off;                                                                               
        Endif;                                                                                      
       Endif;                                                                                       
      /End-Free                                                                                     
     PCrearRegFileS    E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Consulta Registro en el Archivo                //                       
       //------------------------------------------------------------------//                       
     PConsuRegFileP    B                                                                            
     DConsuRegFileP    Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
      /Free                                                                                         
       Clear Rdato9;                                                                                
       Clear Rdato10;                                                                               
       Dow Salir = *Off;                                                                            
       Chain(n) (Pcia:Pcbnc1:Pcdes) Vttbcpf;                                                        
       If %Found(Vttbcpf);                                                                          
        Pcia   = Bcpcia;                                                                            
        Pcbnc  = Bcpcba;                                                                            
        Pdbncp = Bcpban;                                                                            
        Pestp  = Bcpnom;                                                                            
        Ptrip  = Bcppir;                                                                            
        Ptrfp  = Bcppfr;                                                                            
        Ptride = Bcpidr;                                                                            
        Ptprip = Bcppit;                                                                            
        Ptprfp = Bcppft;                                                                            
        Pttrid = Bcpidt;                                                                            
        Pposip = Bcppiv;                                                                            
        Pposfp = Bcppfv;                                                                            
        Pnrop  = Bcpnde;                                                                            
        Pdforp = *Blanks;                                                                           
        Chain(n) Bcpffe Vtmffbf;                                                                    
        If %Found(Vtmffbf);                                                                         
         Pdforp = Ffbdes;                                                                           
        Endif;                                                                                      
        Pspd   = Bcpsde;                                                                            
        Pcrep  = Bcpcev;                                                                            
        Premp  = Bcprev;                                                                            
        Pcdip  = Bcppic;                                                                            
        Pcdfp  = Bcppfc;                                                                            
        Pccep  = Bcpcec;                                                                            
        Pcrmp  = Bcprec;                                                                            
        Pfip   = Bcppif;                                                                            
        Pffp   = Bcppff;                                                                            
        Pcfec  = Bcpffe;                                                                            
        Pca1if = Bcppi1;                                                                            
        Pca1ff = Bcppf1;                                                                            
        Pcmtx = Bcpcc1;                                                                             
        Pnca1p = Bcpnc1;                                                                            
        Pca2if = Bcppi2;                                                                            
        Pca2ff = Bcppf2;                                                                            
        Pnca2p = Bcpnc2;                                                                            
        Exfmt Rdato9;                                                                               
        If  *In10 = *On;                                                                            
         Dow Salir = *Off;                                                                          
          Pidr   = Bcpidc;                                                                          
          Ptpi   = Bcptic;                                                                          
          Ptpf   = Bcptfc;                                                                          
          Pnrdc  = Bcpndc;                                                                          
          Pspdc  = Bcpsdc;                                                                          
          Pcrec  = Bcpcrc;                                                                          
          Prmpc  = Bcprpc;                                                                          
          Exfmt Rdato10;                                                                            
          If Salir = *On;                                                                           
           Salir = *Off;                                                                            
           Leave;                                                                                   
          Endif;                                                                                    
         Enddo;                                                                                     
        Endif;                                                                                      
        Endif;                                                                                      
        Enddo;                                                                                      
        If Salir = *On;                                                                             
         Salir = *Off;                                                                              
        Endif;                                                                                      
      /End-Free                                                                                     
     PConsuRegFileP    E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Consulta Registro en el Archivo                //                       
       //------------------------------------------------------------------//                       
     PConsuRegFileS    B                                                                            
     DConsuRegFileS    Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
      /Free                                                                                         
       Clear Rdato11;                                                                               
       Clear Rdato12;                                                                               
       Dow Salir = *Off;                                                                            
       Chain(n) (Pcia:Pcbnc1:Pcdes) Vttbcsf;                                                        
       If %Found(Vttbcsf);                                                                          
        Pcia   = Bcscia;                                                                            
        Pcbnc  = Bcscba;                                                                            
        Pdbncp = Bcsban;                                                                            
        Pestp  = Bcsnom;                                                                            
        Psepp =  Bcssep;                                                                            
        Ptrip  = Bcspor;                                                                            
        Ptride  = Bcsidr;                                                                           
        Ptprip = Bcspot;                                                                            
        Pttrid = Bcsidt;                                                                            
        Pposip = Bcspov;                                                                            
        Pnrod  = Bcsndv;                                                                            
        Chain(n) Bcsffe Vtmffbf;                                                                    
        If %Found(Vtmffbf);                                                                         
         Pdforp = Ffbdes;                                                                           
        Endif;                                                                                      
        Pspd   = Bcssdv;                                                                            
        Pcrep  = Bcscev;                                                                            
        Premp  = Bcsrev;                                                                            
        Pcdip  = Bcspoc;                                                                            
        Pccep  = Bcscec;                                                                            
        Pcrmp  = Bcsrec;                                                                            
        Pfip   = Bcspof;                                                                            
        Pcfec  = Bcsffe;                                                                            
        Pca1if = Bcspc1;                                                                            
        Pcmtx = Bcscc1;                                                                             
        Pnca1p = Bcsnc1;                                                                            
        Pca2if = Bcspc2;                                                                            
        Pnca2p = Bcsnc2;                                                                            
        Exfmt Rdato11;                                                                              
        If  *In10 = *On;                                                                            
         Dow Salir = *Off;                                                                          
          Pidr   = Bcsidc;                                                                          
          Ptpi   = Bcsptc;                                                                          
          Pnrdc  = Bcsndc;                                                                          
          Pspdc  = Bcssdc;                                                                          
          Pcrec  = Bcscrc;                                                                          
          Prmpc  = Bcsrpc;                                                                          
          Exfmt Rdato12;                                                                            
          If Salir = *On;                                                                           
           Salir = *Off;                                                                            
           Leave;                                                                                   
          Endif;                                                                                    
         Enddo;                                                                                     
        Endif;                                                                                      
        Endif;                                                                                      
        Enddo;                                                                                      
        If Salir = *On;                                                                             
         Salir = *Off;                                                                              
        Endif;                                                                                      
      /End-Free                                                                                     
     PConsuRegFileS    E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Modifica Registro en el Archivo                //                       
       //------------------------------------------------------------------//                       
     PModifiRegFileP   B                                                                            
     DModifiRegFileP   Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DGsw              s              1a   Inz(*Blanks)                                             
      /Free                                                                                         
       Clear Rdato4;                                                                                
       Clear Rdato5;                                                                                
       BuscarRegFileP();                                                                            
       Dow Salir = *Off;                                                                            
        Pdbncp = Pdbnc1;                                                                            
        Pestp = Pdest1;                                                                             
        *In29 = *On;                                                                                
        *In51 = *On;                                                                                
        Exfmt Rdato4;                                                                               
        *In51 = *Off;                                                                               
        If *In79 = *On;                                                                             
         Gsw = 'S';                                                                                 
        Endif;                                                                                      
        ValidarCamp();                                                                              
        If Pforp <> *Zeros And Salir = *Off And Cancelar = *Off And                                 
        *In10 = *Off;                                                                               
         PintarSflFc();                                                                             
         Iter;                                                                                      
        Endif;                                                                                      
        Dow *In10 = *On And Salir = *Off And Cancelar = *Off;                                       
        *In29 = *On;                                                                                
        Exfmt Rdato5;                                                                               
        If *In79 = *On;                                                                             
         Gsw = 'S';                                                                                 
        Endif;                                                                                      
        ValidarCamp1();                                                                             
        If *In08 = *On;                                                                             
         Psw = 'M';                                                                                 
         *In08 = *Off;                                                                              
        If Gsw <> *Blanks;                                                                          
         CrearRegFileP();                                                                           
         Gsw = *Blanks;                                                                             
        Else;                                                                                       
         *In08 = *Off;                                                                              
         Salir = *On;                                                                               
         *In35 = *On;                                                                               
         Exfmt Rdato1;                                                                              
         *In35 = *Off;                                                                              
        Endif;                                                                                      
        Endif;                                                                                      
        If Cancelar = *On;                                                                          
         Cancelar = *Off;                                                                           
         Leave;                                                                                     
        Else;                                                                                       
         *In10 = *On;                                                                               
        Endif;                                                                                      
        Enddo;                                                                                      
        *In10 = *Off;                                                                               
        If Salir = *On;                                                                             
         Leave;                                                                                     
        Endif;                                                                                      
        // Crea Registro en el Archivo de Posiciones                                                
        If *In08 = *On;                                                                             
         Psw = 'M';                                                                                 
         *In08 = *Off;                                                                              
        If Gsw <> *Blanks;                                                                          
         CrearRegFileP();                                                                           
         Gsw = *Blanks;                                                                             
        Else;                                                                                       
         *In08 = *Off;                                                                              
         Salir = *On;                                                                               
         *In35 = *On;                                                                               
         Exfmt Rdato1;                                                                              
         *In35 = *Off;                                                                              
        Endif;                                                                                      
        Endif;                                                                                      
        If Cancelar = *On;                                                                          
         Clear Rdato4;                                                                              
         Clear Rdato5;                                                                              
         BuscarRegFileP();                                                                          
         Iter;                                                                                      
        Endif;                                                                                      
       Enddo;                                                                                       
       If Salir = *On And *In08 = *Off;                                                             
        Salir = *Off;                                                                               
       Endif;                                                                                       
      /End-Free                                                                                     
     PModifiRegFileP   E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Consulta Registro en el Archivo                //                       
       //------------------------------------------------------------------//                       
     PBuscarRegFileP   B                                                                            
     DBuscarRegFileP   Pi                                                                           
      /Free                                                                                         
       Chain(n) (Pcia:Pcbnc1:Pcdes) Vttbcpf;                                                        
       If %Found(Vttbcpf);                                                                          
        Pdbncp = Bcpban;                                                                            
        Pestp  = Bcpnom;                                                                            
        Ptrip  = Bcppir;                                                                            
        Ptrfp  = Bcppfr;                                                                            
        Ptride = Bcpidr;                                                                            
        Ptprip = Bcppit;                                                                            
        Ptprfp = Bcppft;                                                                            
        Pttrid = Bcpidt;                                                                            
        Pposip = Bcppiv;                                                                            
        Pposfp = Bcppfv;                                                                            
        Pnrop  = Bcpnde;                                                                            
        If  Bcpcc1 = 'Si';                                                                          
        Pcomp =  1;                                                                                 
        Else;                                                                                       
        Pcomp =  2;                                                                                 
        Endif;                                                                                      
        Pspd   = Bcpsde;                                                                            
        Pcrep  = Bcpcev;                                                                            
        Premp  = Bcprev;                                                                            
        Pcdip  = Bcppic;                                                                            
        Pcdfp  = Bcppfc;                                                                            
        Pccep  = Bcpcec;                                                                            
        Pcrmp  = Bcprec;                                                                            
        Pfip   = Bcppif;                                                                            
        Pffp   = Bcppff;                                                                            
        Pcfec  = Bcpffe;                                                                            
        Pca1if = Bcppi1;                                                                            
        Pca1ff = Bcppf1;                                                                            
        If Bcpcc1 = 'Si';                                                                           
         Pcomp = 01;                                                                                
        Else;                                                                                       
         Pcomp = 02;                                                                                
        Endif;                                                                                      
        Pnca1p = Bcpnc1;                                                                            
        Pca2if = Bcppi2;                                                                            
        Pca2ff = Bcppf2;                                                                            
        Pnca2p = Bcpnc2;                                                                            
        Pidr   = Bcpidc;                                                                            
        Ptpi   = Bcptic;                                                                            
        Ptpf   = Bcptfc;                                                                            
        Pnrdc  = Bcpndc;                                                                            
        Pspdc  = Bcpsdc;                                                                            
        Pcrec  = Bcpcrc;                                                                            
        Prmpc  = Bcprpc;                                                                            
        Pdforp = *Blanks;                                                                           
        Chain(n) Bcpffe Vtmffbf;                                                                    
        If %Found(Vtmffbf);                                                                         
         Pdforp = Ffbdes;                                                                           
        Endif;                                                                                      
        Endif;                                                                                      
      /End-Free                                                                                     
     PBuscarRegFileP   E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Consulta Registro en el Archivo                //                       
       //------------------------------------------------------------------//                       
     PBuscarRegFileS   B                                                                            
     DBuscarRegFileS   Pi                                                                           
      /Free                                                                                         
       Chain(n) (Pcia:Pcbnc1:Pcdes) Vttbcsf;                                                        
       If %Found(Vttbcsf);                                                                          
        Pcia   = Bcscia;                                                                            
        Pcbnc  = Bcscba;                                                                            
        Pdbncp = Bcsban;                                                                            
        Pestp  = Bcsnom;                                                                            
        Psepp =  Bcssep;                                                                            
        Ptrip  = Bcspor;                                                                            
        Ptride  = Bcsidr;                                                                           
        Ptprip = Bcspot;                                                                            
        Pttrid = Bcsidt;                                                                            
        Pposip = Bcspov;                                                                            
        Pnrod  = Bcsndv;                                                                            
        Pspd   = Bcssdv;                                                                            
        Pcrep  = Bcscev;                                                                            
        Premp  = Bcsrev;                                                                            
        Pcdip  = Bcspoc;                                                                            
        Pccep  = Bcscec;                                                                            
        Pcrmp  = Bcsrec;                                                                            
        Pfip   = Bcspof;                                                                            
        Pcfec  = Bcsffe;                                                                            
        Pca1if = Bcspc1;                                                                            
        If Bcscc1 = 'Si';                                                                           
         Pcomp = 01;                                                                                
        Else;                                                                                       
         Pcomp = 02;                                                                                
        Endif;                                                                                      
        Pnca1p = Bcsnc1;                                                                            
        Pca2if = Bcspc2;                                                                            
        Pnca2p = Bcsnc2;                                                                            
        Pidr   = Bcsidc;                                                                            
        Ptpi   = Bcsptc;                                                                            
        Pnrdc  = Bcsndc;                                                                            
        Pspdc  = Bcssdc;                                                                            
        Pcrec  = Bcscrc;                                                                            
        Prmpc  = Bcsrpc;                                                                            
        Pdforp = *Blanks;                                                                           
        Chain(n) Bcsffe Vtmffbf;                                                                    
        If %Found(Vtmffbf);                                                                         
         Pdforp = Ffbdes;                                                                           
        Endif;                                                                                      
       Endif;                                                                                       
      /End-Free                                                                                     
     PBuscarRegFileS   E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Modifica Registro en el Archivo                //                       
       //------------------------------------------------------------------//                       
     PModifiRegFileS   B                                                                            
     DModifiRegFileS   Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DGsw              s              1a   Inz(*Blanks)                                             
      /Free                                                                                         
       Clear Rdato6;                                                                                
       Clear Rdato7;                                                                                
       Pcomp = 2;                                                                                   
       BuscarRegFileS();                                                                            
       Dow Salir = *Off;                                                                            
        Pdbncp = Bcsban;                                                                            
        Pestp  = Bcsnom;                                                                            
       //  Pdbncp = Pdbnc1;                                                                         
        //Pestp = Pdest1;                                                                           
        *In29 = *On;                                                                                
        *In51 = *On;                                                                                
        Exfmt Rdato6;                                                                               
        *In51 = *Off;                                                                               
        ValidarCamp2();                                                                             
        If *In79 = *On;                                                                             
         Gsw = 'S';                                                                                 
        Endif;                                                                                      
        If Pforp <> *Zeros And Salir = *Off And Cancelar = *Off And                                 
        *In10 = *Off;                                                                               
         PintarSflFc();                                                                             
         Iter;                                                                                      
        Endif;                                                                                      
        Dow *In10 = *On And Salir = *Off And Cancelar = *Off;                                       
        If *In79 = *On;                                                                             
         Gsw = 'S';                                                                                 
        Endif;                                                                                      
        *In29 = *On;                                                                                
        Exfmt Rdato7;                                                                               
        If *In08 = *On;                                                                             
         Psw = 'M';                                                                                 
         *In08 = *Off;                                                                              
        If Gsw <> *Blanks;                                                                          
         Psw = 'M';                                                                                 
         CrearRegFileS();                                                                           
         Gsw = *Blanks;                                                                             
        Else;                                                                                       
         *In08 = *Off;                                                                              
         Salir = *On;                                                                               
         *In35 = *On;                                                                               
         Exfmt Rdato1;                                                                              
         *In35 = *Off;                                                                              
        Endif;                                                                                      
        Endif;                                                                                      
        If Cancelar = *On;                                                                          
         Cancelar = *Off;                                                                           
         Leave;                                                                                     
        Else;                                                                                       
         *In10 = *On;                                                                               
        Endif;                                                                                      
        Enddo;                                                                                      
        *In10 = *Off;                                                                               
        If Salir = *On;                                                                             
         Leave;                                                                                     
        Endif;                                                                                      
        // Crea Registro en el Archivo de Posiciones                                                
        If *In08 = *On;                                                                             
         Psw = 'M';                                                                                 
         *In08 = *Off;                                                                              
        If Gsw <> *Blanks;                                                                          
         Psw = 'M';                                                                                 
         CrearRegFileS();                                                                           
         Gsw = *Blanks;                                                                             
        Else;                                                                                       
         *In08 = *Off;                                                                              
         Salir = *On;                                                                               
         *In35 = *On;                                                                               
         Exfmt Rdato1;                                                                              
         *In35 = *Off;                                                                              
        Endif;                                                                                      
        Endif;                                                                                      
        If Cancelar = *On;                                                                          
         Clear Rdato6;                                                                              
         Clear Rdato7;                                                                              
         BuscarRegFileS();                                                                          
         Iter;                                                                                      
        Endif;                                                                                      
       Enddo;                                                                                       
       If Salir = *On And *In08 = *Off;                                                             
        Salir = *Off;                                                                               
       Endif;                                                                                       
      /End-Free                                                                                     
     PModifiRegFileS   E                                                                            
