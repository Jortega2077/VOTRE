***************************************************************************                         
     **                                                                   *                         
     ** Programa      : VTMCCCCR                                          *                         
     ** Descripciòn:  : Revisar Pagos Resumen por Banco                   *                         
     ** Proyecto      : Menú de Opciones Opi Paises                       *                         
     ** Autor         : (Jortega)                                         *                         
     ** Fecha Creaciòn: 29 de Enero de 2009                               *                         
     **                                                                   *                         
     **********************************************************************                         
     ** Definición de Directivas de Compilación                                                     
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
                                                                                                    
     **Definiciòn de Archivos de Trabajo                                                            
     FVttrbsf   Uf   e           k Disk                                                             
     FVttbcsf   If   e           k Disk                                                             
     FVttbcpf   If   e           k Disk                                                             
     FVtmbncf   If   e           k Disk                                                             
                                                                                                    
     ** Definiciòn de Archivo de Pantallas                                                          
     FVtbnci3p  Cf   e             Workstn                                                          
     F                                     Sfile(Rdatos2:Nrr)                                       
                                                                                                    
     ** Definiciòn de Variables de Trabajo                                                          
     D $C              c                   ''''                                                     
     DPfile            s             10a   Inz(*Blanks)                                             
     DPnbnc            s             40a   Inz(*Blanks)                                             
     DXcia             s              3    Inz(*Blanks)                                             
     DXpais            s             20    Inz(*Blanks)                                             
     DPtr              s               *   Inz(%Addr(*In))                                          
     DXcdb             s              2s 0 Inz(*Zeros)                                              
     DWfile            s             10a                                                            
     DPosix            s              4s 0 Inz(*Zeros)                                              
     DPcbnx1           s              2a                                                            
     DPdarc1           s           1000a                                                            
     DPbanK1           s              3a   Inz(*Blanks)                                             
     DNror             s              4s 0 Inz(*Zeros)                                              
     DAsw              s              1a   Inz(*Blanks)                                             
     DNroRtt           s              9s 0 Inz(*Zeros)                                              
     DAuxFile          s             10a   Inz(*Blanks)                                             
     D w_userPasw      s            200                                                             
     D w_servidor      s            100                                                             
     D w_Ruta          s            200                                                             
                                                                                                    
     ** Definiciòn de Estructuras de Trabajo                                                        
     DIndic            Ds                  Based(Ptr)                                               
     DSalir                            N   Overlay(Indic:3)                                         
     DCancelar                         N   Overlay(Indic:12)                                        
     DSflDsp                           N   Overlay(Indic:85)                                        
     DSflDspCtl                        N   Overlay(Indic:80)                                        
     DSflClr                           N   Overlay(Indic:75)                                        
     DSflEnd                           N   Overlay(Indic:40)                                        
                                                                                                    
     ** Definiciòn de Estructuras de Trabajo                                                        
     DFields           Ds                                                                           
     DQrbsfca                              Like(Rbsfca)                                             
     DQrbsnba                              Like(Rbsnba)                                             
     DQrbsnes                              Like(Rbsnes)                                             
     DQrbstre                              Like(Rbstre)                                             
     DQrbsvto                              Like(Rbsvto)                                             
     DQrbsnar                              Like(Rbsnar)                                             
     DQrbsarc                              Like(Rbsarc)                                             
     DQrbscba                              Like(Rbscba)                                             
     DQrbsrar                              Like(Rbsrar)                                             
     DQrbsces                              Like(Rbsces)                                             
                                                                                                    
     DFields1          Ds                                                                           
     DYrbsfca                              Like(Rbsfca)                                             
     DYrbsnba                              Like(Rbsnba)                                             
     DYrbsnes                              Like(Rbsnes)                                             
     DYrbstre                              Like(Rbstre)                                             
     DYrbsvto                              Like(Rbsvto)                                             
     DYrbsnar                              Like(Rbsnar)                                             
     DYrbsarc                              Like(Rbsarc)                                             
     DYrbscba                              Like(Rbscba)                                             
     DYrbsrar                              Like(Rbsrar)                                             
     DYrbsces                              Like(Rbsces)                                             
     DYRbsinp                              Like(Rbsinp)                                             
     DYRbsini                              Like(Rbsini)                                             
                                                                                                    
     ** Definiciòn de Prototipos de Trabajo                                                         
     DLlenarSfl        Pr                                                                           
     DBorraSfl         Pr                                                                           
     DPintarSfl        Pr                                                                           
     DBorrarFile       Pr                                                                           
     DBorrarRegF       Pr                                                                           
     DBorrarRegSfl     Pr                                                                           
     DActualRegF       Pr                                                                           
     DActualRegFTotal  Pr                                                                           
     DPrContarReg      Pr              n                                                            
     DPrVerificarBancoTrabajado...                                                                  
     D                 Pr                                                                           
     DPrContarRegFile  Pr             9  0                                                          
     DPrReconteo       Pr             9  0                                                          
                                                                                                    
     DPgmPais          Pr                  Extpgm('VOTREP00/VTMCSBBR')                              
     DXcia                            3                                                             
     DXpais                          20                                                             
                                                                                                    
     DPgmVerificar     Pr                  Extpgm('VOTREP00/VTMCCC9L')                              
     DPcia                            3a                                                            
     DPfile                          10a                                                            
     DPnbnc                          40a                                                            
     DPcbnx1                          2a                                                            
     DPdarc1                       1000a                                                            
     DPbanK                           3a                                                            
     DNror                            4s 0                                                          
                                                                                                    
     DPrBorrarFiles    Pr                  Extpgm('VOTREP00/VTMCCCPR')                              
     DPcia                            3a                                                            
                                                                                                    
     DPgmNotas600      Pr                  Extpgm('VOTREP00/VTMCCP4L')                              
     DPcia                            3a                                                            
                                                                                                    
     DComando          Pr                  ExtPgm('QCMDEXC')                                        
     DCmd                           500A   Options(*VarSize) Const                                  
     DCmdLen                         15P 5 Const                                                    
                                                                                                    
     DVTMCC21L         Pr                  extpgm('VOTREP00/VTMCC21L')                              
     D  Pcia                          3                                                             
     D  Pserv                       100                                                             
       End-pr;                                                                                      
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
         If Pcia = '600';                                                                           
          PgmNotas600(Pcia);                                                                        
         Endif;                                                                                     
         Exec sql                                                                                   
         Set Option Commit = *None, Dlyprp = *Yes;                                                  
         Dow Salir = *Off;                                                                          
          PrBorrarFiles(Pcia);                                                                      
          Pfecha = %Char(%Date());                                                                  
          PintarSfl();                                                                              
          If Nrr <= *Zeros And Salir = *Off And Cancelar = *Off;                                    
           ActualRegFTotal();                                                                       
           *In38 = *On;                                                                             
           Exfmt Rdato1;                                                                            
           *In38 = *Off;                                                                            
           Iter;                                                                                    
          Endif;                                                                                    
          If Salir = *On;                                                                           
           Leave;                                                                                   
          Endif;                                                                                    
          If Cancelar = *On;                                                                        
           Posix = *Zeros;                                                                          
           Clear Rcontrol2;                                                                         
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
      ** Definición de Variable de trabajo                                                          
     DSqlNr1           s            500a   Inz(*Blanks)                                             
     DNro              s              4s 0 Inz(*Zeros)                                              
     DWvalorX          s             15s 2 Inz(*Zeros)                                              
     DWvalor           s             15s 2 Inz(*Zeros)                                              
     DValora           s            500a   Inz(*Blanks)                                             
     DOPosDec          s              4s 0 Inz(*Zeros)                                              
     DTnum             s              4a   Inz(*Blanks)                                             
     DKnum             s              4s 0 Inz(*Zeros)                                              
     DIdx              s              4s 0 Inz(1)                                                   
     DValorVec         s              1a   Dim(100)                                                 
     DValorVecD        s              1a   Dim(100)                                                 
     DIdx0             s              3s 0 Inz(1)                                                   
     DIdx1             s              3s 0 Inz(1)                                                   
     DYRbsarc          s             10a   Inz(*Blanks)                                             
     DAuxBan           s             10a   Inz(*Blanks)                                             
     DAuxBanSFL        s             10a   Inz(*Blanks)                                             
     DZRbsarc          s             10a   Inz(*Blanks)                                             
     DZRbsfca          s             10a   Inz(*Blanks)                                             
     DZRbsrar          s           1000a   Inz(*Blanks)                                             
     DZRbsfar          s             10a   Inz(*Blanks)                                             
     DZRbstre          s              4  0 Inz(*Zeros)                                              
     DZRbsvto          s             15  2 Inz(*Zeros)                                              
     DZRbsnar          s                   Like(Rbsnar)                                             
     DZRbsnba          s             40    Inz(*Blanks)                                             
     DZRbshca          s             10    Inz(*Blanks)                                             
     DNroRtt           s              9s 0 Inz(*Zeros)                                              
     DFechaAc          s             10a   Inz(*Blanks)                                             
     DFechaMn          s             10a   Inz(*Blanks)                                             
     DNfilenA          s            500a   Inz(*Blanks)                                             
     DFehcarB          s             10a   Inz(*Blanks)                                             
     DHorcarC          s              8a   Inz(*Blanks)                                             
      /Free                                                                                         
       Asw = *Blanks;                                                                               
       BorraSfl();                                                                                  
       Exec Sql                                                                                     
       Select Replace(Char(Current date, Iso), '-', '/'),                                           
              Replace(Char(Current date - 2 Days, Iso), '-', '/')                                   
       Into :FechaAc, :FechaMn                                                                      
       From Sysibm/Sysdummy1;                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Declare CX1 Cursor For                                                                      
        Select Rbsarc, Rbsfca, Rbsrar, Rbsfar, Rbstre, Rbsvto, Rbsnar                               
               , Rbsnba, Rbshca                                                                     
        From Vttrbsf                                                                                
        Where ((Rbsinp = 'P') Or (                                                                  
        Rbsinp = 'T' And Rbsini = 'G'))                                                             
        And Rbscia = :Pcia And                                                                      
        Rbsfca Between :FechaMn And :FechaAc And Rbsarc <> ' '                                      
        Order By Rbsnba, Rbsfca;                                                                    
                                                                                                    
       Exec Sql                                                                                     
        Open CX1;                                                                                   
                                                                                                    
       Exec Sql                                                                                     
        Fetch CX1 Into :ZRbsarc, :ZRbsfca, :ZRbsrar,                                                
                       :ZRbsfar, :ZRbstre, :ZRbsvto, :ZRbsnar,                                      
                       :ZRbsnba, :ZRbshca;                                                          
                                                                                                    
       Dow Sqlcod <> 100 And Sqlcod >= *Zeros;                                                      
        NroRtt = *Zeros;                                                                            
        SqlNr1 = *Blanks;                                                                           
        SqlNr1 = 'Select Nfilen, Fehcar, Horcar, Count(0) ' +                                       
                 'From Qgpl/' + %Trim(ZRbsarc) +                                                    
                 ' Group by Nfilen, Fehcar, Horcar With nc ';                                       
                                                                                                    
        Exec Sql                                                                                    
         Prepare Cur15a From :SqlNr1;                                                               
                                                                                                    
        Exec Sql                                                                                    
         Declare Cursor15a Cursor For Cur15a;                                                       
                                                                                                    
        Exec Sql                                                                                    
         Open Cursor15a;                                                                            
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor15a Into :NfilenA, :FehcarB, :HorcarC, :NroRtt;                                 
                                                                                                    
        If Sqlcod <> 100 And Sqlcod >= *Zeros;                                                      
         If NroRtt > *Zeros;                                                                        
          If AuxBan <> ZRbsarc;                                                                     
           AuxBan = ZRbsarc;                                                                        
           Exec Sql                                                                                 
            Update Vttrbsf Set Rbsinp = 'P'                                                         
            Where Rbsinp = 'T' And Rbscia = :Pcia                                                   
            And Trim(Rbsarc) = Trim(:ZRbsarc) And                                                   
            Rbsnar = :NfilenA And                                                                   
            Rbsfca = :FehcarB And                                                                   
            Rbshca = :HorcarC;                                                                      
          Endif;                                                                                    
         ElseIf NroRtt <= *Zeros;                                                                   
           Exec Sql                                                                                 
            Update Vttrbsf Set Rbsinp = 'T'                                                         
            Where Rbsinp = 'P' And Rbscia = :Pcia                                                   
            And Trim(Rbsarc) = Trim(:ZRbsarc)                                                       
            And Rbsfca = :ZRbsfca And                                                               
            Trim(Rbsrar) = Trim(:ZRbsrar);                                                          
         Endif;                                                                                     
        ElseIf Sqlcod = 100 Or Sqlcod < *Zeros;                                                     
         If NroRtt <= *Zeros;                                                                       
          Exec Sql                                                                                  
           Update Vttrbsf Set Rbsinp = 'T'                                                          
           Where Rbsinp = 'P' And Rbscia = :Pcia                                                    
           And Trim(Rbsarc) = Trim(:ZRbsarc)                                                        
           And Rbsfca = :ZRbsfca And                                                                
           Trim(Rbsrar) = Trim(:ZRbsrar);                                                           
         Endif;                                                                                     
        Endif;                                                                                      
                                                                                                    
        Exec Sql                                                                                    
         Close Cursor15a;                                                                           
                                                                                                    
       Exec Sql                                                                                     
        Fetch CX1 Into :ZRbsarc, :ZRbsfca, :ZRbsrar,                                                
                       :ZRbsfar, :ZRbstre, :ZRbsvto, :ZRbsnar,                                      
                       :ZRbsnba, :ZRbshca;                                                          
       Enddo;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close CX1;                                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Declare CX Cursor For                                                                       
        Select Distinct Rbsarc                                                                      
        From Vttrbsf                                                                                
        Where ((Rbsinp = 'P' And Rbstre = 0 And                                                     
        Rbsvto = 0) Or (Rbsinp = 'P'))                                                              
        And Rbscia = :Pcia;                                                                         
                                                                                                    
       Exec Sql                                                                                     
        Open CX;                                                                                    
                                                                                                    
       Exec Sql                                                                                     
        Fetch CX Into :YRbsarc;                                                                     
                                                                                                    
       NroRtt = *Zeros;                                                                             
       Dow Sqlcod <> 100 And Sqlcod >= *Zeros;                                                      
                                                                                                    
        SqlNr1 = *Blanks;                                                                           
        SqlNr1 = 'Select Count(0) As NroRtt' +                                                      
                 ' From Qgpl/' + %Trim(YRbsarc) + ' Wiht nc ';                                      
                                                                                                    
        Exec Sql                                                                                    
         Prepare Cur15 From :SqlNr1;                                                                
                                                                                                    
        Exec Sql                                                                                    
         Declare Cursor15 Cursor For Cur15;                                                         
                                                                                                    
        Exec Sql                                                                                    
         Open Cursor15;                                                                             
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor15 Into :NroRtt;                                                                
                                                                                                    
        If Sqlcod <> 100 And Sqlcod >= *Zeros;                                                      
         If NroRtt <= *Zeros;                                                                       
          Exec Sql                                                                                  
           Delete From Vttrbsf                                                                      
           Where Rbsinp = 'P' And Rbstre = 0 And                                                    
           Rbsvto = 0 And Rbsarc = ' ' And Trim(Rbsarc) = Trim(:YRbsarc);                           
         Endif;                                                                                     
        Endif;                                                                                      
                                                                                                    
        Exec Sql                                                                                    
         Close Cursor15;                                                                            
                                                                                                    
        Exec Sql                                                                                    
         Fetch CX Into :YRbsarc;                                                                    
       Enddo;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close CX;                                                                                   
       //PrVerificarBancoTrabajado();                                                               
                                                                                                    
       Exec Sql                                                                                     
        Declare C1 Cursor For                                                                       
        Select Rbsfca, Rbsnba, Rbsnes, Rbstre, Rbsvto, Rbsnar, Rbsarc,                              
               Rbscba, Rbsrar, Rbsces                                                               
        From Vttrbsf                                                                                
        Where Rbsinp <> 'T' And Rbscia = :Pcia And Rbsfar <> ' '                                    
        //And (Trim(Rbsarc) = Trim(:ZRbsarc) Or                                                     
        //Trim(Rbsarc) = Trim(:YRbsarc))                                                            
        Order By Rbsfca Desc;                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Open C1;                                                                                    
                                                                                                    
       Exec Sql                                                                                     
        Fetch C1 Into :Fields;                                                                      
                                                                                                    
       If Sqlcod <> 100 And Sqlcod >= *Zeros;                                                       
        Chain(n) (Pcia:Qrbscba:Qrbsces) Vttbcsf;                                                    
        If %Found(Vttbcsf);                                                                         
         Asw = 'S';                                                                                 
         OPosDec = Bcsndv;                                                                          
        Else;                                                                                       
         Chain(n) (Pcia:Qrbscba:Qrbsces) Vttbcpf;                                                   
          If %Found(Vttbcpf);                                                                       
           Asw = *Blanks;                                                                           
           OPosDec = Bcpnde;                                                                        
          Endif;                                                                                    
        Endif;                                                                                      
       Endif;                                                                                       
        AuxBanSFL = *Blanks;                                                                        
        Dow Sqlcod <> 100;                                                                          
        Nro = *Zeros;                                                                               
        Wvalor = *Zeros;                                                                            
        WvalorX = *Zeros;                                                                           
        Wfile = *Blanks;                                                                            
        Wfile = Qrbsarc;                                                                            
        SqlNr1 = *Blanks;                                                                           
        If Wfile <> *Blanks;                                                                        
        // Se Consulta si el Archivo Tiene Registros con Inconsistencias                            
        SqlNr1 = 'Select Valor From Qgpl/' + %Trim(Wfile) +                                         
                 ' For Read Only';                                                                  
                                                                                                    
        Exec Sql                                                                                    
         Prepare Cur From :SqlNr1;                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor1 Asensitive Scroll Cursor For Cur;                                           
                                                                                                    
       Exec Sql                                                                                     
        Open Cursor1;                                                                               
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor1 Into :Valora;                                                                 
                                                                                                    
        Dow Sqlcod <> 100 And Sqlcod >= *Zeros;                                                     
         Nro += 1;                                                                                  
         If %Len(%Trim(Valora)) > *Zeros;                                                           
                                                                                                    
      /End-Free                                                                                     
     C                   MoveA     *All' '       ValorVec                                           
     C                   MoveA     *All' '       ValorVecD                                          
     C                   MoveA     Valora        ValorVec                                           
      /Free                                                                                         
         For Idx0 = 1 To %Elem(ValorVec);                                                           
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
                                                                                                    
          WvalorX = *Zeros;                                                                         
          Monitor;                                                                                  
           WvalorX = %Dec(Valora:11:2);                                                             
          On-Error;                                                                                 
           WvalorX = *Zeros;                                                                        
          Endmon;                                                                                   
           If OPosDec > *Zeros;                                                                     
            Tnum = '1';                                                                             
            For Idx = 1 to OPosDec;                                                                 
             Tnum = %Trim(Tnum) + '0';                                                              
            EndFor;                                                                                 
            Knum = %Int(%Trim(Tnum));                                                               
            WvalorX = WvalorX/Knum;                                                                 
           Endif;                                                                                   
           If OPosDec > *Zeros And Pcia = '350';                                                    
            Tnum = '1';                                                                             
            For Idx = 1 to OPosDec;                                                                 
             Tnum = %Trim(Tnum) + '0';                                                              
            EndFor;                                                                                 
            Knum = %Int(%Trim(Tnum));                                                               
            WvalorX = WvalorX/Knum;                                                                 
           Endif;                                                                                   
          Wvalor = Wvalor + WvalorX;                                                                
         Endif;                                                                                     
        Exec Sql                                                                                    
         Fetch Cursor1 Into :Valora;                                                                
        Enddo;                                                                                      
                                                                                                    
        Exec Sql                                                                                    
         Close Cursor1;                                                                             
                                                                                                    
        If Nro > *Zeros;                                                                            
         If Nro > *Zeros;                                                                           
         If AuxBanSFL <> ZRbsarc;                                                                   
          AuxBan = Qrbsnba;                                                                         
          Pop = *Blanks;                                                                            
          Pfcar = Qrbsfca;                                                                          
          Pnban = Qrbsnba;                                                                          
          Pnro = Nro;                                                                               
          Evalr Pvlr = %Trim(%Editc(Wvalor:'N'));                                                   
          Pnest1 = Qrbsnes;                                                                         
          Pxfilep = Qrbsarc;                                                                        
          Pcbnx = Qrbscba;                                                                          
          Pdarc = Qrbsrar;                                                                          
          Nrr += 1;                                                                                 
          Posi = Nrr;                                                                               
          Write Rdatos2;                                                                            
         Endif;                                                                                     
         Endif;                                                                                     
        EndIf;                                                                                      
                                                                                                    
         If Nrr <= *Zeros;                                                                          
          BorrarFile();                                                                             
          ActualRegF();                                                                             
         Endif;                                                                                     
         Endif;                                                                                     
                                                                                                    
         Exec Sql                                                                                   
          Fetch C1 Into :Fields;                                                                    
        Enddo;                                                                                      
                                                                                                    
       Exec Sql                                                                                     
        Close C1;                                                                                   
                                                                                                    
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
       Nror = *Zeros;                                                                               
       Nrr = *Zeros;                                                                                
       SflClr = *On;                                                                                
       Write Rcontrol2;                                                                             
       SflClr = *Off;                                                                               
      /End-Free                                                                                     
     PBorraSfl         E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Pinta el Sub Archivo de la Pantalla            //                       
       //------------------------------------------------------------------//                       
     PPintarSfl        B                                                                            
     DPintarSfl        Pi                                                                           
      /Free                                                                                         
       LlenarSfl();                                                                                 
       If Posix > *Zeros;                                                                           
        Posic = Posix;                                                                              
       Endif;                                                                                       
       If Nrr > *Zeros;                                                                             
        *In30 = *On;                                                                                
       Endif;                                                                                       
       Ppais = Xpais;                                                                               
       SflDspCtl = *On;                                                                             
       Write Rteclas;                                                                               
       Exfmt Rcontrol2;                                                                             
       SflDspCtl = *Off;                                                                            
       *In30 = *Off;                                                                                
       If Salir = *Off And Cancelar = *Off And Nrr > *Zeros;                                        
        Readc Rdatos2;                                                                              
         Dow Not %Eof();                                                                            
          Select;                                                                                   
           When Pop = '1';                                                                          
            PbanK1 = *Blanks;                                                                       
            Chain(n) (%Int(Pcbnx)) Vtmbncf;                                                         
            If %Found(Vtmbncf);                                                                     
             PbanK1 = %Trim(Bnctnd);                                                                
            Endif;                                                                                  
            Pfile = Pxfilep;                                                                        
            Pnbnc = Pnban;                                                                          
            Pcbnx1 = %Char(Pcbnx);                                                                  
            Pdarc1 = Pdarc;                                                                         
            PgmVerificar(Pcia:Pfile:Pnbnc:Pcbnx1:Pdarc1:PbanK1:Nror);                               
            If Nror = *Zeros;                                                                       
             Posix = Posi;                                                                          
            Else;                                                                                   
             Posix = Nror;                                                                          
            Endif;                                                                                  
           When Pop = '4';                                                                          
            Chain(n) (Pcia:Pcbnx:Pdarc) Vttrbsf;                                                    
            If %Found(Vttrbsf);                                                                     
             If Rbsinp = 'P';                                                                       
              BorrarRegSfl();                                                                       
              Posix = 1;                                                                            
             Else;                                                                                  
              *In31 = *On;                                                                          
              Exfmt Rdato1;                                                                         
              *In31 = *Off;                                                                         
            Endif;                                                                                  
            Endif;                                                                                  
          Endsl;                                                                                    
          Pop = *Blanks;                                                                            
          Update Rdatos2;                                                                           
         Readc Rdatos2;                                                                             
        Enddo;                                                                                      
        Endif;                                                                                      
       LlenarSfl();                                                                                 
      /End-Free                                                                                     
     PPintarSfl        E                                                                            
                                                                                                    
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
         BorrarFile();                                                                              
         BorrarRegF();                                                                              
         *In39 = *On;                                                                               
         Exfmt Rdato3;                                                                              
         *In39 = *Off;                                                                              
        Endif;                                                                                      
       Enddo;                                                                                       
      /End-Free                                                                                     
     PBorrarRegSfl     E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Borra el Archivo del Banco Creado en la Qgpl   //                       
       //------------------------------------------------------------------//                       
     PBorrarFile       B                                                                            
     DBorrarFile       Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DCmd              s           3000a   Inz(*Blanks)                                             
     DNro              s              9s 0 Inz(*Zeros)                                              
     DOsw              s               n   Inz(*Off)                                                
      /Free                                                                                         
                                                                                                    
        If (Popc = 's' Or Popc = 'S') And Pop = '4';                                                
         Osw = *On;                                                                                 
        Endif;                                                                                      
                                                                                                    
        If Popc <> 's' And Popc <> 'S' And Pop <> '4';                                              
         Popc = *Blanks;                                                                            
         Pop = *Blanks;                                                                             
         Osw = *Off;                                                                                
         Cmd = *Blanks;                                                                             
         Cmd = 'Select Count(0) ' +                                                                 
               'From Qgpl/' + %Trim(Pxfilep);                                                       
                                                                                                    
         Exec Sql                                                                                   
          Prepare CurKK From :Cmd;                                                                  
                                                                                                    
         Exec Sql                                                                                   
          Declare CursorKK Cursor For CurKK;                                                        
                                                                                                    
         Exec Sql                                                                                   
          Open CursorKK;                                                                            
                                                                                                    
         Exec Sql                                                                                   
          Fetch CursorKK Into :Nro;                                                                 
                                                                                                    
         If Sqlcod <> 100 And SqlCod >= *Zeros;                                                     
          If Nro <= *Zeros;                                                                         
           If PrContarReg()= *On;                                                                   
            Osw = *On;                                                                              
           Endif;                                                                                   
          Endif;                                                                                    
         ElseIf Sqlcod = 100 Or SqlCod < *Zeros;                                                    
          If PrContarReg()= *On;                                                                    
           Osw = *On;                                                                               
          Endif;                                                                                    
         Endif;                                                                                     
       Endif;                                                                                       
                                                                                                    
       If Osw = *On;                                                                                
        Pop = *Blanks;                                                                              
        Osw = *Off;                                                                                 
        Cmd = *Blanks;                                                                              
        Cmd = 'Drop Table Qgpl/' + %Trim(Pxfilep);                                                  
                                                                                                    
        Exec Sql                                                                                    
         Prepare Cur From :Cmd;                                                                     
                                                                                                    
        Exec Sql                                                                                    
         Execute Cur;                                                                               
                                                                                                    
        Cmd = *Blanks;                                                                              
        Cmd = 'Delete From Qgpl/' + %Trim(Pxfilep);                                                 
                                                                                                    
        Exec Sql                                                                                    
         Execute Immediate :Cmd;                                                                    
                                                                                                    
        Monitor;                                                                                    
         Clear Cmd;                                                                                 
         Cmd = 'Dltf File(Qgpl/' + %Trim(Pxfilep) + ')';                                            
         Callp(E) Comando(Cmd:%Size(Cmd));                                                          
        On-Error;                                                                                   
        Endmon;                                                                                     
                                                                                                    
       Endif;                                                                                       
      /End-Free                                                                                     
     PBorrarFile       E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Borra Registro del Archivo Vttrbsf             //                       
       //------------------------------------------------------------------//                       
     PBorrarRegF       B                                                                            
     DBorrarRegF       Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DSqlStmt          s           1000a   Inz(*Blanks)                                             
      /Free                                                                                         
        Chain (Pcia:Pcbnx:Pdarc) Vttrbsf;                                                           
         If %Found(Vttrbsf);                                                                        
          Pr_FtpRecuperarDocumentoBanco();                                                          
          Delete Regrbs;                                                                            
                                                                                                    
          SqlStmt = *Blanks;                                                                        
          SqlStmt = 'Delete From Qtemp/NotasDeta';                                                  
                                                                                                    
          Exec Sql                                                                                  
           Execute Immediate :SqlStmt;                                                              
                                                                                                    
         Endif;                                                                                     
      /End-Free                                                                                     
     PBorrarRegF       E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Actualiza Registro del Archivo Vttrbsf         //                       
       //------------------------------------------------------------------//                       
     PActualRegF       B                                                                            
     DActualRegF       Pi                                                                           
      /Free                                                                                         
        Chain (Pcia:Pcbnx:Pdarc) Vttrbsf;                                                           
         If %Found(Vttrbsf);                                                                        
          Rbsinp = 'T';                                                                             
          Update Regrbs %Fields(Rbsinp);                                                            
         Endif;                                                                                     
      /End-Free                                                                                     
     PActualRegF       E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Actualiza Registro del Archivo Vttrbsf         //                       
       // Total                                                            //                       
       //------------------------------------------------------------------//                       
     PActualRegFTotal  B                                                                            
     DActualRegFTotal  Pi                                                                           
      /Free                                                                                         
       Exec Sql                                                                                     
        Update Votrea00/Vttrbsf Set Rbsinp = 'T'                                                    
        Where Rbsinp =  'P' And Rbscia = :Pcia;                                                     
      /End-Free                                                                                     
     PActualRegFTotal  E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Verifica si el Banco ya Fue Trabajado no       //                       
       // Muestre este Banco                                                                        
       //------------------------------------------------------------------//                       
     PPrVerificarBancoTrabajado...                                                                  
     P                 B                                                                            
     DPrVerificarBancoTrabajado...                                                                  
     D                 Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DCmd              s           1000a   Inz(*Blanks)                                             
      /Free                                                                                         
       Exec sql                                                                                     
        Declare C1j Cursor For                                                                      
        Select Rbsfca, Rbsnba, Rbsnes, Rbstre, Rbsvto, Rbsnar, Rbsarc,                              
               Rbscba, Rbsrar, Rbsces, Rbsinp, Rbsini                                               
        From Vttrbsf                                                                                
        Where Rbsinp <> 'T' And Rbscia = :Pcia And Rbsfar <> ' '                                    
        Order By Rbsfca Desc;                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Open C1j;                                                                                   
                                                                                                    
       Exec Sql                                                                                     
        Fetch C1j Into :Fields1;                                                                    
                                                                                                    
       AuxFile = *Blanks;                                                                           
       Dow Sqlcod <> 100 And Sqlcod >= *Zeros;                                                      
                                                                                                    
        If PrContarRegFile() <> Yrbstre And YRbsinp = 'P' And                                       
           YRbsini = 'G' And PrReconteo >= 2;                                                       
                                                                                                    
         Exec Sql                                                                                   
          Update Votrea00/Vttrbsf Set Rbsinp = 'T'                                                  
          Where Rbsinp =  'P' And Rbscia = :Pcia                                                    
          And Rbscba = :Yrbscba And Rbsnar = :Yrbsnar;                                              
        Endif;                                                                                      
       Exec Sql                                                                                     
        Fetch C1j Into :Fields1;                                                                    
       Enddo;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close C1j;                                                                                  
      /End-Free                                                                                     
     PPrVerificarBancoTrabajado...                                                                  
     P                 E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Determina si el Archivo tiene Almenos un Regis //                       
       // tro                                                              //                       
       //------------------------------------------------------------------//                       
     PPrContarReg      B                                                                            
     DPrContarReg      Pi              n                                                            
      ** Definición de Variables de Trabajo                                                         
     DOsw              s               n   Inz(*Off)                                                
     DCedula           s             30a   Inz(*Blanks)                                             
     DNroCom           s              9s 0 Inz(*Zeros)                                              
     DSqlNr1           s            500a   Inz(*Blanks)                                             
      /Free                                                                                         
        SqlNr1 = *Blanks;                                                                           
        SqlNr1 = 'Select Cedula ' +                                                                 
                 ' From Qgpl/' + %Trim(Pxfilep);                                                    
                                                                                                    
        Exec Sql                                                                                    
         Prepare Cur22 From :SqlNr1;                                                                
                                                                                                    
        Exec Sql                                                                                    
         Declare Cursor22 Cursor For Cur22;                                                         
                                                                                                    
        Exec Sql                                                                                    
         Open Cursor22;                                                                             
                                                                                                    
        Exec Sql                                                                                    
         Fetch Cursor22 Into :Cedula;                                                               
                                                                                                    
        NroCom = *Zeros;                                                                            
        Dow Sqlcod <> 100 And Sqlcod >= *Zeros;                                                     
          NroCom += 1;                                                                              
         Exec Sql                                                                                   
          Fetch Cursor22 Into :Cedula;                                                              
        Enddo;                                                                                      
                                                                                                    
        Exec Sql                                                                                    
         Close Cursor22;                                                                            
       If NroCom <= *Zeros;                                                                         
        Osw = *On;                                                                                  
       Endif;                                                                                       
       Return Osw;                                                                                  
      /End-Free                                                                                     
     PPrContarReg      E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Calcula el Número de registros del Archivo     //                       
       //------------------------------------------------------------------//                       
     PPrContarRegFile  B                                                                            
     DPrContarRegFile  Pi             9  0                                                          
      ** Definición de Variables de Trabajo                                                         
     DCmd              s           1000a   Inz(*Blanks)                                             
     DNroRtt1          s              9  0 Inz(*Zeros)                                              
      /Free                                                                                         
        Cmd = *Blanks;                                                                              
        Cmd = 'Select Count(0) As NroRtt1' +                                                        
              ' From Qgpl/' + %Trim(YRbsarc) +' with nc ';                                          
                                                                                                    
        Exec Sql                                                                                    
         Prepare Cur155 From :Cmd;                                                                  
                                                                                                    
        Exec Sql                                                                                    
         Declare Cursor155 Cursor For Cur155;                                                       
                                                                                                    
        Exec Sql                                                                                    
         Open Cursor155;                                                                            
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor155 Into :NroRtt1;                                                              
                                                                                                    
        If Sqlcod = 100 Or Sqlcod < *Zeros;                                                         
         NroRtt1 = *Zeros;                                                                          
        Endif;                                                                                      
                                                                                                    
        Exec Sql                                                                                    
         Close Cursor155;                                                                           
                                                                                                    
        Return NroRtt1;                                                                             
      /End-Free                                                                                     
     PPrContarRegFile  E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Calcula el Número de registros del Archivo     //                       
       //------------------------------------------------------------------//                       
     PPrReconteo       B                                                                            
     DPrReconteo       Pi             9  0                                                          
      ** Definición de Variables de Trabajo                                                         
     DCmd              s           1000a   Inz(*Blanks)                                             
     DNroRtt2          s              9  0 Inz(*Zeros)                                              
      /Free                                                                                         
       If %Trim(AuxFile) <> %Trim(YRbsarc);                                                         
        AuxFile = YRbsarc;                                                                          
        Cmd = *Blanks;                                                                              
        Cmd = 'Select Count(0) As NroRtt2 ' +                                                       
              'From Votrea00/Vttrbsf ' +                                                            
              'Where Rbsinp <> ''T''' +                                                             
              ' And Rbscia = ' + ''''+Pcia+'''' + ' And Rbsfar <> '' '''+                           
              ' And Rbsarc = ' + '''' + %Trim(YRbsarc)+ '''';                                       
                                                                                                    
        Exec Sql                                                                                    
         Prepare Cur156 From :Cmd;                                                                  
                                                                                                    
        Exec Sql                                                                                    
         Declare Cursor156 Cursor For Cur156;                                                       
                                                                                                    
        Exec Sql                                                                                    
         Open Cursor156;                                                                            
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor156 Into :NroRtt2;                                                              
                                                                                                    
        If Sqlcod = 100 Or Sqlcod < *Zeros;                                                         
         NroRtt2 = *Zeros;                                                                          
        Endif;                                                                                      
                                                                                                    
        Exec Sql                                                                                    
         Close Cursor156;                                                                           
                                                                                                    
        Endif;                                                                                      
        Return NroRtt2;                                                                             
      /End-Free                                                                                     
     PPrReconteo       E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Prototipo Pr_FtpRecuperarDocumentoBanco...                       //                       
       //           Recupera Documento procesado a Virsv/Recaudo           //                       
       //------------------------------------------------------------------//                       
     PPr_FtpRecuperarDocumentoBanco...                                                              
     P                 B                                                                            
     DPr_FtpRecuperarDocumentoBanco...                                                              
     D                 Pi                                                                           
     DCmd              s           1000a   Inz(*Blanks)                                             
      /Free                                                                                         
        Clear w_userPasw;                                                                           
        Clear w_servidor;                                                                           
        Clear w_Ruta;                                                                               
        Exec Sql                                                                                    
        Select Trim(FTPUSR)||' '||Trim(FTPPWD) , FTPSVR , FTPRTA                                    
        Into :w_userPasw ,:w_servidor,:w_Ruta                                                       
        From VTmAsbf                                                                                
        inner join Vtmftpf on Trim(FTPAPL)= Trim(Asbapl) and FTPEVT = Asbevt                        
        Where ASBCIA = :RBSCIA                                                                      
          And ASBCDB = :RBSCBA                                                                      
          And ASBCDE = :RBSCES                                                                      
          And ASBEST = ' ';                                                                         
       If Sqlcod = *Zeros;                                                                          
          Exec Sql                                                                                  
          Delete from Vtwftpf6;                                                                     
                                                                                                    
          Exec Sql                                                                                  
           Insert into Vtwftpf6 (FTPDES) Values(:w_userPasw);                                       
          Exec Sql                                                                                  
           Insert into Vtwftpf6 (FTPDES)                                                            
           Values('rename'||' '||Trim(:w_Ruta)||'Tmp/'||Trim(:RBSNAR)||' '||                        
                  Trim(:w_Ruta)||'/'||Trim(:RBSNAR));                                               
                                                                                                    
          Exec Sql                                                                                  
           Insert into Vtwftpf6 (FTPDES)                                                            
           Values('Delete'||' '||Trim(:w_Ruta)||'Tmp/'||Trim(:RBSNAR) );                            
          Exec Sql                                                                                  
           Insert into Vtwftpf6 (FTPDES) Values('Close');                                           
                                                                                                    
          Exec Sql                                                                                  
           Insert into Vtwftpf6 (FTPDES) Values('Quit');                                            
                                                                                                    
          Exec Sql                                                                                  
           Insert into Vtwftpf6 (FTPDES) Values('Exit');                                            
          Clear cmd;                                                                                
          Cmd ='SBMJOB CMD(CALL PGM(VOTREP00/VTMCC21L) PARM('+                                      
               $C +%trim(Pcia)+ $C + ' ' +  $C +%Trim(w_servidor)+$C+                               
               ' )) JOB(EXCFTP6) JOBQ(JOBQQRY) ';                                                   
          Callp(E) Comando(Cmd:%Size(Cmd));                                                         
          //VTMCC21L(Pcia:w_servidor);                                                              
       EndIf;                                                                                       
      /End-Free                                                                                     
     PPr_FtpRecuperarDocumentoBanco...                                                              
     P                 E                                                                            
