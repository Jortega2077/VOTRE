***************************************************************************                         
     **                                                                   *                         
     ** Programa      : VTMCCCSR                                          *                         
     ** Descripciòn:  : Consulta Pedidos Liberados                        *                         
     ** Proyecto      : Menú de Opciones Opi Paises                       *                         
     ** Autor         : (Jortega)                                         *                         
     ** Fecha Creaciòn: 24 de Septiembre de 2010                          *                         
     **                                                                   *                         
     **********************************************************************                         
     ** Definición de Directivas de Compilación                                                     
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
                                                                                                    
     ** Definiciòn de Archivo de Pantallas                                                          
     FVtcpli0p  Cf   e             Workstn                                                          
     F                                     Sfile(Rdatos:Nrr)                                        
                                                                                                    
     ** Definiciòn de Variables de Trabajo                                                          
     DXcia             s              3    Inz(*Blanks)                                             
     DXpais            s             20    Inz(*Blanks)                                             
     DPtr              s               *   Inz(%Addr(*In))                                          
     DStrSql           s           1000a   Inz(*Blanks)                                             
     DAuxZn            s              3a   Inz(*Blanks)                                             
     DCntPedos         s              5s 0 Inz(*Zeros)                                              
     DVlrPedos         s             12s 2 Inz(*Zeros)                                              
     DCntPedosT        s              5s 0 Inz(*Zeros)                                              
     DVlrPedosT        s             12s 2 Inz(*Zeros)                                              
                                                                                                    
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
     DOhsls1                         10a                                                            
     DOhordÑ                          9s 0                                                          
     DOhcstÑ                          9s 0                                                          
     DMlname                         20                                                             
     DValor                          12  2                                                          
     DOhstcd                          1                                                             
     DOhprlv                          3                                                             
     DOhcrhl                          1                                                             
                                                                                                    
     ** Definiciòn de Prototipos de Trabajo                                                         
     DLlenarSfl        Pr                                                                           
     DBorraSfl         Pr                                                                           
     DPintarSfl        Pr                                                                           
                                                                                                    
     DPgmPais          Pr                  Extpgm('VOTREP00/VTMCSBBR')                              
     DXcia                            3                                                             
     DXpais                          20                                                             
                                                                                                    
     DPgmNegativos     Pr                  Extpgm('VOTREP00/VTACRC3L')                              
     DXcia                            3                                                             
     DXsku                           15                                                             
                                                                                                    
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
                                                                                                    
        If Salir = *On;                                                                             
         Leave;                                                                                     
        Endif;                                                                                      
                                                                                                    
        If Cancelar = *On;                                                                          
         Clear Rcontrol;                                                                            
        Endif;                                                                                      
                                                                                                    
       If Salir = *Off And Pcampa <> *Blanks;                                                       
        LlenarSfl();                                                                                
       Endif;                                                                                       
                                                                                                    
       If Nrr <= *Zeros And Salir = *Off And Cancelar = *Off And                                    
          Pcampa <> *Blanks;                                                                        
        Pcampa = *Blanks;                                                                           
        *In38 = *On;                                                                                
        Exfmt Rdato1;                                                                               
        *In38 = *Off;                                                                               
        Iter;                                                                                       
       Endif;                                                                                       
                                                                                                    
       If Nrr <= *Zeros And Salir = *Off And Cancelar = *Off And                                    
          Pcampa = *Blanks;                                                                         
        Pcampa = *Blanks;                                                                           
        *In37 = *On;                                                                                
        Exfmt Rdato1;                                                                               
        *In37 = *Off;                                                                               
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
       StrSql = *Blanks;                                                                            
       StrSql = 'Select Ohsls1, OhordÑ, OhcstÑ, Mlname, (Ohmram+Ohtxa2) A+                          
                s Valor, Ohstcd, Ohprlv, Ohcrhl ' +                                                 
                'From Opf' + Pcia + '/Lorhdr01 Inner Join ' +                                       
                'Opf' + Pcia + '/Lmllst01 On OhcstÑ = MlcstÑ ' +                                    
                'Where Ohmeda = ' + '''' + Pcampa + '''' + ' And ' +                                
                'Ohrcst = ''A''' + ' And Ohmram > 0 And Ohcrhl <> ''Y''' +                          
                ' Order By Mlzipc';                                                                 
                                                                                                    
        Exec Sql                                                                                    
         Prepare Cur0 From :StrSql;                                                                 
                                                                                                    
        Exec Sql                                                                                    
         Declare Cursor0 Cursor For Cur0;                                                           
                                                                                                    
        Exec Sql                                                                                    
         Open Cursor0;                                                                              
                                                                                                    
        Exec Sql                                                                                    
         Fetch Cursor0 Into :Fields;                                                                
                                                                                                    
        CntPedosT = *Zeros;                                                                         
        VlrPedosT = *Zeros;                                                                         
        Dow Sqlcod <> 100 And SqlCod >= *Zeros;                                                     
         AuxZn = Ohsls1;                                                                            
         CntPedos = *Zeros;                                                                         
         VlrPedos = *Zeros;                                                                         
         Dow Sqlcod <> 100 And SqlCod >= *Zeros  And AuxZn = Ohsls1;                                
          *In30 = *Off;                                                                             
          *In31 = *Off;                                                                             
          *In32 = *Off;                                                                             
          CntPedos += 1;                                                                            
          VlrPedos = VlrPedos + Valor;                                                              
          Pzn = Ohsls1;                                                                             
          Pod = OhordÑ;                                                                             
          Pcd = OhcstÑ;                                                                             
          Pnb = Mlname;                                                                             
          Evalr Pvr = %Trim(%Editc(Valor:'P'));                                                     
          Psc = Ohstcd;                                                                             
          Pcc = Ohprlv;                                                                             
          Phl = Ohcrhl;                                                                             
          Nrr += 1;                                                                                 
          Write Rdatos;                                                                             
          Exec Sql                                                                                  
           Fetch Cursor0 Into :Fields;                                                              
         Enddo;                                                                                     
          CntPedosT = CntPedosT + CntPedos;                                                         
          VlrPedosT = VlrPedosT + VlrPedos;                                                         
          *In30 = *On;                                                                              
          *In31 = *On;                                                                              
          Pzn = *Blanks;                                                                            
          Pod = *Zeros;                                                                             
          Pcd = *Zeros;                                                                             
          Pnb = 'Total ' + %Trim(%Editc(CntPedos:'Z')) +                                            
                 ' Pds:';                                                                           
          Pvr = %Trim(%Editc(VlrPedos:'P'));                                                        
          Psc = *Blanks;                                                                            
          Pcc = *Blanks;                                                                            
          Phl = *Blanks;                                                                            
          Nrr += 1;                                                                                 
          Write Rdatos;                                                                             
        Enddo;                                                                                      
        If Nrr > *Zeros;                                                                            
         *In30 = *On;                                                                               
         *In32 = *On;                                                                               
         Pzn = *Blanks;                                                                             
         Pod = *Zeros;                                                                              
         Pcd = *Zeros;                                                                              
         Pnb = 'Total Pedidos (' + %Trim(%Editc(CntPedosT:'Z')) +                                   
                ') Vlr:';                                                                           
         Pvr = %Trim(%Editc(VlrPedosT:'P'));                                                        
         Psc = *Blanks;                                                                             
         Pcc = *Blanks;                                                                             
         Phl = *Blanks;                                                                             
         Nrr += 1;                                                                                  
         Write Rdatos;                                                                              
        Endif;                                                                                      
                                                                                                    
        Exec Sql                                                                                    
         Close Cursor0;                                                                             
                                                                                                    
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
      /End-Free                                                                                     
     PPintarSfl        E                                                                            
