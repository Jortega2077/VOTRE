***************************************************************************                         
     **                                                                   *                         
     ** Programa      : VTMCCCORA                                         *                         
     ** Descripciòn:  : Consulta Cantidades Negativas y si hay algun sku  *                         
     **                 entonces envia correo informativo                 *                         
     ** Proyecto      : Negativos                                         *                         
     ** Autor         : Cescobar                                          *                         
     ** Fecha Creaciòn: 20 de Octubre de 2014                             *                         
     **                                                                   *                         
     **********************************************************************                         
     ** Definición de Directivas de Compilación                                                     
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
                                                                                                    
     ** Definiciòn de Archivos                                                                      
     FLCACMP01  If   e           K Disk                                                             
                                                                                                    
     ** Definiciòn de Variables de Trabajo                                                          
     DXcia             s              3    Inz(*Blanks)                                             
     DXpais            s             20    Inz(*Blanks)                                             
     DPtr              s               *   Inz(%Addr(*In))                                          
     DStrSql           s           1000a   Inz(*Blanks)                                             
     DPDeEmail         s            200a   Inz(' ')                                                 
     DPParaEmail       s            200a   Inz(' ')                                                 
     DPRemiteEmail     s            200a   Inz(' ')                                                 
     DPAsuntoEmail     s           2000a   Inz(' ')                                                 
     DPTextEmail       s          10000a   Inz(' ')                                                 
     DPpar             s              1    Inz(' ')                                                 
     DWcont            s              5s 0 Inz(0)                                                   
                                                                                                    
     ** Definiciòn de Estructuras de Trabajo                                                        
                                                                                                    
     ** Definiciòn de Estructuras de Trabajo                                                        
                                                                                                    
     ** Definiciòn de Prototipos de Trabajo                                                         
                                                                                                    
     DPgmPais          Pr                  Extpgm('VOTREP00/VTMCSBBR')                              
     DXcia                            3                                                             
     DXpais                          20                                                             
                                                                                                    
     ** Programa envio mail                                                                         
     DPgmMail          Pr                  Extpgm('VOTREP00/VTMGOI0R')                              
     DPDeEmail                      200a                                                            
     DPParaEmail                    200a                                                            
     DPRemiteEmail                  200a                                                            
     DPAsuntoEmail                 2000a                                                            
     DPTextEmail                  10000a                                                            
                                                                                                    
     DLibreria         Pr                  Extpgm('VOTREP00/VTSPAM0L')                              
     DPcia                            3                                                             
     DPpar                            1                                                             
                                                                                                    
     DMain             Pr                  Extpgm('Main')                                           
     DPLlmd                           1a                                                            
     DPcia                            3a                                                            
     DPind                            1a                                                            
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
     DPLlmd                           1a                                                            
     DPcia                            3a                                                            
     DPind                            1a                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       //              P r o g r a m a   P r i n c i p a l                 //                       
       //------------------------------------------------------------------//                       
      /Free                                                                                         
                                                                                                    
        // Si viene D es la ejecución Diara por lo que consulta todas las                           
        // compañias                                                                                
                                                                                                    
        If PLlmd = 'D';                                                                             
         Setll (*LOVAL) LCACMP01;                                                                   
         Read LCACMP01;                                                                             
         Dow not %eof(LCACMP01);                                                                    
           If CCDELE = ' ' AND CCCMPÑ <> 700 AND CCCMPÑ <> 980 AND                                  
              CCCMPÑ <> 955 AND CCCMPÑ <> 3;                                                        
              Pcia = %Editc(CCCMPÑ:'X');                                                            
              Proceso();                                                                            
           Endif;                                                                                   
         Read LCACMP01;                                                                             
         Enddo;                                                                                     
                                                                                                    
         Else;                                                                                      
          Proceso();                                                                                
        Endif;                                                                                      
                                                                                                    
        *Inlr = *On;                                                                                
      /End-Free                                                                                     
                                                                                                    
       //------------------------------------------------------------------//                       
       // Proceso de busqueda de negativos                                 //                       
       //------------------------------------------------------------------//                       
     PProceso          B                                                                            
     DProceso          Pi                                                                           
      /Free                                                                                         
                                                                                                    
        If PLlmd = 'D';                                                                             
          OpnFiles();                                                                               
        Endif;                                                                                      
                                                                                                    
        // Valida si hay skus negativos                                                             
        StrSql = *Blanks;                                                                           
        StrSql = 'Select Count(*) ' +                                                               
                'From Opf' + Pcia + '/Linqty01 Inner Join ' +                                       
                'Opf' + Pcia + '/Linvnt01 On Ivitem = Iqitem ' +                                    
                'Where (Iqqtym < 0 And Ivstck = ''Y''' + ' And ' +                                  
                'Ivkitp = ''N''' + ') Or (Iqqtya < 0 And Ivstck = ''Y''' +                          
                ' And ' + 'Ivkitp = ''N''' + ')';                                                   
                                                                                                    
        Exec sql                                                                                    
         Prepare C1 from :StrSql;                                                                   
                                                                                                    
        Exec sql                                                                                    
         Declare Cur01 Cursor for C1;                                                               
                                                                                                    
        Exec sql                                                                                    
         Open Cur01;                                                                                
                                                                                                    
        Exec sql                                                                                    
         Fetch Cur01 Into :Wcont;                                                                   
                                                                                                    
        If Wcont <> 0;                                                                              
           Mail();                                                                                  
           Pind = 'S';                                                                              
        Endif;                                                                                      
                                                                                                    
        Exec sql                                                                                    
         Close Cur01;                                                                               
                                                                                                    
          If PLlmd = 'D';                                                                           
            ClsFiles();                                                                             
          Endif;                                                                                    
                                                                                                    
      /End-Free                                                                                     
     PProceso          E                                                                            
       //------------------------------------------------------------------//                       
       // Procedimiento que envia correo informativo                       //                       
       //------------------------------------------------------------------//                       
     PMail             B                                                                            
     DMail             Pi                                                                           
      /Free                                                                                         
                                                                                                    
        PDeEmail = 'LogisticaColombia@leonisa.com';                                                 
        PParaEmail = 'dgomezm@votresas.com';                                                        
        PRemiteEmail = 'Logistica Leonisa';                                                         
        PAsuntoEmail = 'Aviso Negativos Compañia ' + Pcia;                                          
        PTextEmail   = 'Aviso Negativos Compañia ' + Pcia;                                          
                                                                                                    
          PgmMail(PDeEmail:PParaEmail:PRemiteEmail:PAsuntoEmail:                                    
                     PTextEmail);                                                                   
                                                                                                    
      /End-Free                                                                                     
     PMail             E                                                                            
       //------------------------------------------------------------------//                       
       // Procedimiento para abrir archivos                                //                       
       //------------------------------------------------------------------//                       
     POpnFiles         B                                                                            
     DOpnFiles         Pi                                                                           
      /Free                                                                                         
                                                                                                    
        //Adiciona librerias de trabajo                                                             
         Ppar = 'A';                                                                                
         Libreria(Pcia:Ppar);                                                                       
                                                                                                    
      /End-Free                                                                                     
     POpnFiles         E                                                                            
       //------------------------------------------------------------------//                       
       // Procedimiento para cerrar archivos                               //                       
       //------------------------------------------------------------------//                       
     PClsFiles         B                                                                            
     DClsFiles         Pi                                                                           
      /Free                                                                                         
                                                                                                    
        //Adiciona librerias de trabajo                                                             
         Ppar = 'E';                                                                                
         Libreria(Pcia:Ppar);                                                                       
                                                                                                    
      /End-Free                                                                                     
     PClsFiles         E                                                                            
