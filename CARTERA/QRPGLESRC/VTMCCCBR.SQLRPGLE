***************************************************************************                         
     **                                                                   *                         
     ** Programa      : VTMCCCBR                                          *                         
     ** Descripciòn:  : Historia de Bancos Subidos al As/400              *                         
     ** Proyecto      : Menú de Opciones Opi Paises                       *                         
     ** Autor         : (Jortega)                                         *                         
     ** Fecha Creaciòn: 28 de Enero de 2009                               *                         
     **                                                                   *                         
     **********************************************************************                         
     ** Definición de Directivas de Compilación                                                     
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
                                                                                                    
     **Definiciòn de Archivos de Trabajo                                                            
     FVttrbsf   If   e           k Disk                                                             
     FVtmbncf   If   e           k Disk                                                             
                                                                                                    
     ** Definiciòn de Archivo de Pantallas                                                          
     FVtbnci3p  Cf   e             Workstn                                                          
     F                                     Sfile(Rdatos:Nrr)                                        
     F                                     Sfile(Rdatos1:Nrr1)                                      
                                                                                                    
     ** Definiciòn de Variables de Trabajo                                                          
     DXcia             s              3    Inz(*Blanks)                                             
     DXpais            s             20    Inz(*Blanks)                                             
     DPtr              s               *   Inz(%Addr(*In))                                          
     DXcdb             s              2s 0 Inz(*Zeros)                                              
                                                                                                    
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
     DQrbshca                              Like(Rbshca)                                             
     DQrbsnes                              Like(Rbsnes)                                             
     DQrbstre                              Like(Rbstre)                                             
     DQrbsvto                              Like(Rbsvto)                                             
     DQrbsnar                              Like(Rbsnar)                                             
                                                                                                    
     ** Definiciòn de Prototipos de Trabajo                                                         
     DLlenarSfl        Pr                                                                           
     DBorraSfl         Pr                                                                           
     DPintarSfl        Pr                                                                           
     DLlenarSflBn      Pr                                                                           
     DBorraSflBn       Pr                                                                           
     DPintarSflBn      Pr                                                                           
                                                                                                    
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
          If Pban = *Zeros And Salir = *Off And Cancelar = *Off And                                 
             Nrr = *Zeros;                                                                          
           *In30 = *On;                                                                             
          Endif;                                                                                    
          If Pban <> *Zeros And Salir = *Off And Cancelar = *Off;                                   
           PintarSflBn();                                                                           
           Pban = *Zeros;                                                                           
           LlenarSfl();                                                                             
          Endif;                                                                                    
          If Nrr <= *Zeros And Salir = *Off And Pndes <> *Blanks And                                
             Cancelar = *Off;                                                                       
           *In38 = *On;                                                                             
           Exfmt Rdato1;                                                                            
           *In38 = *Off;                                                                            
           Iter;                                                                                    
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
      ** Definición de Variables de Trabajo                                                         
     DFecIni           s             10a                                                            
     DStrDate          s               d                                                            
     DEndDate          s               d                                                            
     DNmeses           s              2s 0 Inz(3)                                                   
      /Free                                                                                         
       EndDate = %Date();                                                                           
       StrDate = EndDate - %Months(Nmeses);                                                         
       FecIni = %Char(StrDate);                                                                     
       BorraSfl();                                                                                  
       Exec Sql                                                                                     
        Declare C1 Scroll Cursor For                                                                
        Select Rbsfca, Rbshca, Rbsnes, Rbstre, Rbsvto, Rbsnar                                       
        From Vttrbsf                                                                                
        Where Rbscba = :Xcdb And Rbscia = :Pcia And                                                 
        Rbsfca >= :FecIni And Rbsfar <> ' '                                                         
        Order By Rbsfca Desc;                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Open C1;                                                                                    
                                                                                                    
       Exec Sql                                                                                     
        Fetch C1 Into :Fields;                                                                      
                                                                                                    
        Dow Sqlcod <> 100 And Sqlcod >= *Zeros;                                                     
         Pfcar = Qrbsfca;                                                                           
         Phcar = Qrbshca;                                                                           
         Pnest = Qrbsnes;                                                                           
         Pnro = Qrbstre;                                                                            
         Evalr Pvlr = %Trim(%Editc(Qrbsvto:'N'));                                                   
         Evalr Pnarc = %Trim(Qrbsnar);                                                              
         Nrr += 1;                                                                                  
         Posi = Nrr;                                                                                
         Write Rdatos;                                                                              
       Exec Sql                                                                                     
        Fetch C1 Into :Fields;                                                                      
        Enddo;                                                                                      
                                                                                                    
       Exec Sql                                                                                     
        Close C1;                                                                                   
                                                                                                    
        If Nrr > *Zeros;                                                                            
         SflEnd = *On;                                                                              
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
       Ppais = Xpais;                                                                               
       SflDspCtl = *On;                                                                             
       Write Rteclas;                                                                               
       Exfmt Rcontrol;                                                                              
       SflDspCtl = *Off;                                                                            
       *In30 = *Off;                                                                                
      /End-Free                                                                                     
     PPintarSfl        E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Llena el Sub Archivo del Sub File              //                       
       //------------------------------------------------------------------//                       
     PLlenarSflBn      B                                                                            
     DLlenarSflBn      Pi                                                                           
      /Free                                                                                         
       BorraSflBn();                                                                                
       Exec Sql                                                                                     
        Declare C2 Scroll Cursor For                                                                
        Select Bnccod, Bncdes                                                                       
        From Vtmbncf                                                                                
        Order By Bncdes;                                                                            
                                                                                                    
       Exec Sql                                                                                     
        Open C2;                                                                                    
                                                                                                    
       Exec Sql                                                                                     
        Fetch C2 Into :Bnccod, :Bncdes;                                                             
                                                                                                    
        Dow Sqlcod <> 100 And Sqlcod >= *Zeros;                                                     
         Pop = *Blanks;                                                                             
         Pcbc = Bnccod;                                                                             
         Pdbc = Bncdes;                                                                             
         Nrr1 += 1;                                                                                 
         Write Rdatos1;                                                                             
       Exec Sql                                                                                     
        Fetch C2 Into :Bnccod, :Bncdes;                                                             
        Enddo;                                                                                      
                                                                                                    
       Exec Sql                                                                                     
        Close C2;                                                                                   
                                                                                                    
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
       *In90 = *Off;                                                                                
       If Salir = *Off And Salir = *Off And Nrr1 > *Zeros;                                          
        Readc Rdatos1;                                                                              
         Dow Not %Eof();                                                                            
          Select;                                                                                   
           When Pop = '1';                                                                          
            Fsw = 'S';                                                                              
            Xcdb  = Pcbc;                                                                           
            Pndes = Pdbc;                                                                           
          Endsl;                                                                                    
          Pop = *Blanks;                                                                            
          Update Rdatos1;                                                                           
         Readc Rdatos1;                                                                             
        Enddo;                                                                                      
        Endif;                                                                                      
       Enddo;                                                                                       
      /End-Free                                                                                     
     PPintarSflBn      E                                                                            
