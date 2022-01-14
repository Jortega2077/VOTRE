      **********************************************************************                        
      *                                                                   **                        
      * Programa      : VTMCCCMR                                          **                        
      * Descripción   : Generar Certificados Cartera                      **                        
      * Proyecto      : Menú de Opciones Opi Paises                       **                        
      * Autor         : (Jortega)                                         **                        
      * Fecha Creación: 23 de Febrero de 2010                             **                        
      *Modificacion 2020 10 06 a Solicitud Area Cartera Maria Carmen      **                        
      *se inhabilitan las opciones de Generar paz y salvo y referen ccial **                        
      *Hecho por Martha Cecilia restrepo londoño                                                    
      *                                                                   **                        
      **********************************************************************                        
     ** Definición de Directivas de Compilación                                                     
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
                                                                                                    
     **Definiciòn de Archivos de Trabajo                                                            
     FLmllst01  If   e           k Disk                                                             
                                                                                                    
     ** Definiciòn de Archivo de Pantallas                                                          
     FVtcpci3p  Cf   e             Workstn                                                          
                                                                                                    
     ** Definiciòn de Variables de Trabajo                                                          
     DPtr              s               *   Inz(%Addr(*In))                                          
     DWcedula          s              9s 0 Inz(*Zeros)                                              
     DTced             s             30                                                             
     DTcns             s              9                                                             
     DUcia             s             10    Inz(*Blanks)                                             
     DPciaR            s             10a   Inz(*Blanks)                                             
     DPres             s              2a   Inz(*Blanks)                                             
     DPano             s              4                                                             
                                                                                                    
     ** Definiciòn de Estructuras de Trabajo                                                        
     DIndic            Ds                  Based(Ptr)                                               
     DSalir                            N   Overlay(Indic:3)                                         
     DCancelar                         N   Overlay(Indic:12)                                        
                                                                                                    
     ** Definiciòn de Prototipos de Trabajo                                                         
     DPrPrincipal      Pr                                                                           
     DPrMailList       Pr                                                                           
     DPrCertificados   Pr                                                                           
     DPTipo                           2  0                                                          
                                                                                                    
     DConversion       Pr                  Extpgm('VOTREP00/VTMCSBCR')                              
     DYcia                            3                                                             
     DTced                           30                                                             
     DTcns                            9                                                             
                                                                                                    
     DCompaÑia         Pr                  Extpgm('VOTREP00/VTMCSBFR')                              
     DUcia                           10                                                             
     Dkres                            2                                                             
     DkciaR                          10                                                             
                                                                                                    
     DListaAno         Pr                  Extpgm('VOTREP00/VTSPLIAR')                              
     DPano                            4                                                             
                                                                                                    
     DMain             Pr                  Extpgm('Main')                                           
     DPcia                            3a                                                            
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
     DPcia                            3a                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       //              P r o g r a m a   P r i n c i p a l                 //                       
       //------------------------------------------------------------------//                       
      /Free                                                                                         
       PrPrincipal();                                                                               
       *Inlr = *On;                                                                                 
      /End-Free                                                                                     
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento Principal del Programa Ingreso de Información      //                       
       //------------------------------------------------------------------//                       
     PPrPrincipal      B                                                                            
     DPrPrincipal      Pi                                                                           
      /Free                                                                                         
       Clear Rdato2;                                                                                
       Dow Salir = *Off;                                                                            
        Write Rblanco;                                                                              
        Exfmt Rdato2;                                                                               
        If Salir = *On;                                                                             
         Leave;                                                                                     
        Endif;                                                                                      
        If Cancelar = *On;                                                                          
         Tced = *Blanks;                                                                            
         Tcns = *Blanks;                                                                            
         Clear Rdato2;                                                                              
         Iter;                                                                                      
        Endif;                                                                                      
        If Salir = *Off And Cancelar = *off;                                                        
         PrMailList();                                                                              
        Endif;                                                                                      
        If Popc <> *Zeros;                                                                          
         Select;                                                                                    
         When Popc = 01;                                                                            
          PrCertificados(Popc);                                                                     
         When Popc = 02;                                                                            
          PrCertificados(Popc);                                                                     
         When Popc = 03;                                                                            
          PrCertificados(Popc);                                                                     
         EndSl;                                                                                     
         Clear Rdato2;                                                                              
        Endif;                                                                                      
       Enddo;                                                                                       
      /End-Free                                                                                     
     PPrPrincipal      E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Consulta si la Cedula Ingresada es la de una   //                       
       // Compradora                                                       //                       
       //------------------------------------------------------------------//                       
     PPrMailList       B                                                                            
     DPrMailList       Pi                                                                           
      /Free                                                                                         
       Wcedula = *Zeros;                                                                            
       Tced = *Blanks;                                                                              
       Tcns = *Blanks;                                                                              
       Tced = Pcedula;                                                                              
       Conversion(Pcia:Tced:Tcns);                                                                  
       If %Len(%Trim(Tcns)) > *Zeros;                                                               
        Wcedula = %Int(Tcns);                                                                       
       Endif;                                                                                       
       If Wcedula > *Zeros;                                                                         
        Chain(n) Wcedula Lmllst01;                                                                  
        If %Found(Lmllst01);                                                                        
         Pnombre = Mlname;                                                                          
        Else;                                                                                       
         *In30 = *On;                                                                               
         Exfmt Rdato1;                                                                              
         *In30 = *Off;                                                                              
         Clear Rdato2;                                                                              
        Endif;                                                                                      
       Else;                                                                                        
        *In31 = *On;                                                                                
        Exfmt Rdato1;                                                                               
        *In31 = *Off;                                                                               
        Clear Rdato2;                                                                               
       Endif;                                                                                       
      /End-Free                                                                                     
     PPrMailList       E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Genera Certificados de Cartera                 //                       
       //------------------------------------------------------------------//                       
     PPrCertificados   B                                                                            
     DPrCertificados   Pi                                                                           
     DPTipo                           2  0                                                          
      ** Definición de Variables de Trabajo                                                         
     DWPat             s            200    Inz(*Blanks)                                             
     DWPatE            s            200    Inz(*Blanks)                                             
     DCmd              s           3000a   Inz(*Blanks)                                             
     DUser             s             10a   Inz(*User)                                               
     DIcia             s              3a   Inz(*Blanks)                                             
                                                                                                    
     ** Definiciòn de Prototipos de Trabajo                                                         
     DQcmdexc          Pr                  Extpgm('QCMDEXC')                                        
     D                             3000a   Const Options(*Varsize)                                  
     D                               15p 5 Const                                                    
     D                                3a   Const Options(*Nopass)                                   
                                                                                                    
      /Free                                                                                         
                                                                                                    
       Ucia = Pcia;                                                                                 
       CompaÑia(Ucia:Pres:PciaR);                                                                   
       Icia = PciaR;                                                                                
       Select;                                                                                      
       When Ptipo = 01;                                                                             
        // Si el País es Colombia                                                                   
        If Pcia <= '002';                                                                           
         WpatE = '\\noteslei\Dat\Opi\Opi_Bajo_AS400\ejecutar' + ' '                                 
               + Icia + ' ' + %Trim(%Editc(Wcedula:'Z')) + ' ' + 'CRC' + ' R';                      
        Endif;                                                                                      
        // Si el País es Costa Rica                                                                 
        If Pcia = '100';                                                                            
         WpatE = '\\noteslei\Dat\Opi\Opi_Bajo_AS400\ejecutar' + ' '                                 
               + Icia + ' ' + %Trim(%Editc(Wcedula:'Z')) + ' ' + 'CTC' + ' ' +                      
               'R' + ' ' + User;                                                                    
        Endif;                                                                                      
       When Ptipo = 02;                                                                             
        // Si el País es Colombia                                                                   
        If Pcia <= '002';                                                                           
         WpatE = '\\noteslei\Dat\Opi\Opi_Bajo_AS400\ejecutar' + ' '                                 
               + Icia + ' ' + %Trim(%Editc(Wcedula:'Z')) + ' ' + 'CRC' + ' P';                      
        Endif;                                                                                      
        // Si el País es Costa Rica                                                                 
        If Pcia = '100';                                                                            
         WpatE = '\\noteslei\Dat\Opi\Opi_Bajo_AS400\ejecutar' + ' '                                 
               + Icia + ' ' + %Trim(%Editc(Wcedula:'Z')) + ' ' + 'PZC' + ' ' +                      
               'P' + ' ' + User;                                                                    
        Endif;                                                                                      
       When Ptipo = 03;                                                                             
        ListaAno(PANO);                                                                             
        If Pano <> ' ';                                                                             
         // Si el País es Colombia                                                                  
         If Pcia <= '002';                                                                          
          WpatE = '\\noteslei\Dat\Opi\Opi_Bajo_AS400\ejecutar' + ' '                                
               + Icia + ' ' + %Trim(Pcedula) + ' '                                                  
               + 'NCV'+ ' ' + Pano;                                                                 
         Endif;                                                                                     
         // Si el País es Costa Rica                                                                
         If Pcia = '100';                                                                           
          WpatE = '\\noteslei\Dat\Opi\Opi_Bajo_AS400\ejecutar' + ' '                                
               + Icia + ' ' + %Trim(Pcedula) + ' '                                                  
               + 'CVC'+ ' ' + Pano + ' ' + User;                                                    
         Endif;                                                                                     
        Else;                                                                                       
         Return;                                                                                    
        EndIf;                                                                                      
       EndSl;                                                                                       
       Cmd = *Blanks;                                                                               
       Cmd = 'STRPCO PCTA(*NO)';                                                                    
       Callp(E) Qcmdexc(Cmd:%Size(Cmd));                                                            
       WPat = '''' +  %Trim(WPatE);                                                                 
       Cmd = *Blanks;                                                                               
       Cmd = 'STRPCCMD PCCMD(' + %Trim(WPat) +                                                      
              '''' + ') PAUSE(*NO)';                                                                
       Callp(E) Qcmdexc(Cmd:%Size(Cmd));                                                            
      /End-Free                                                                                     
     PPrCertificados   E                                                                            
