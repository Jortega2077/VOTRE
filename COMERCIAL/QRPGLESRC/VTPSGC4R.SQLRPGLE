      ***********************************************************************                       
      ** Programa      : VTPSGC4R                                          **                       
      ** Descripciòn   : PGM que Borra Registros Dobles en Contactos       **                       
      ** Autor         : Leonel Mauricio Parra Suárez-PersonalSoft S.A.    **                       
      ** Responsable   : Julian Ortega                                     **                       
      ** Fecha Creación: 03 de Abril de 2017                               **                       
      ***********************************************************************                       
      ** Definición de Directivas de Compilación                                                    
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
                                                                                                    
     DNro              s              5  0 Inz(*Zeros)                                              
     DOonro            s              5  0 Inz(*Zeros)                                              
     DLagcto           s              9  0 Inz(*Zeros)                                              
     DLagcia           s              3a   Inz(*Blanks)                                             
     DLagcdg           s              9  0 Inz(*Zeros)                                              
     DLagzna           s              3a   Inz(*Blanks)                                             
     DRrn              s              9  0 Inz(*Zeros)                                              
     DRzgficW          s             10a   Inz(*Blanks)                                             
     DIfecha           s             10a   Inz(*Blanks)                                             
     DOsec             s              1a   Inz(*Blanks)                                             
                                                                                                    
     DMain             Pr                  Extpgm('Main')                                           
     DPcia                            3a                                                            
     DPzona                           3a                                                            
     DPcamp                           4a                                                            
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
     DPcia                            3a                                                            
     DPzona                           3a                                                            
     DPcamp                           4a                                                            
                                                                                                    
     ** Definiciòn de Prototipos de Trabajo                                                         
     DPrValidar        Pr                                                                           
     DPrSectorContac   Pr             1a                                                            
     DWcia                            3a                                                            
     DWcedula                         9  0                                                          
                                                                                                    
       //------------------------------------------------------------------//                       
       //              P r o g r a m a   P r i n c i p a l                 //                       
       //------------------------------------------------------------------//                       
      /Free                                                                                         
       Exec Sql                                                                                     
        Declare Cur0 Cursor For                                                                     
        Select Distinct(Count(Lagcto)), Max(Lagcto)                                                 
        From Votrea00/Vtwlagf                                                                       
        Where Lagcia = :Pcia And                                                                    
              Lagzna = :Pzona And                                                                   
              Lagcmp = :Pcamp And                                                                   
              Lagtpv In ('A','E')                                                                   
        Group By Lagcto                                                                             
        Having Count(Lagcto) > 1;                                                                   
                                                                                                    
       Exec Sql                                                                                     
        Open Cur0;                                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cur0 Into :Nro, :Lagcto;                                                              
                                                                                                    
       Dow Sqlcod <> 100 And Sqlcod >= *Zeros;                                                      
         PrValidar();                                                                               
        Exec Sql                                                                                    
         Fetch Cur0 Into :Nro, :Lagcto;                                                             
       Enddo;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close Cur0;                                                                                 
                                                                                                    
       Return;                                                                                      
      /End-Free                                                                                     
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Valida el tipo de Contacto a Borrar            //                       
       //------------------------------------------------------------------//                       
     PPrValidar        B                                                                            
     DPrValidar        Pi                                                                           
     DHnro             s              5  0 Inz(1)                                                   
      /Free                                                                                         
       Exec Sql                                                                                     
        Declare Cur1 Cursor For                                                                     
        Select Lagcia, Lagcdg, Lagzna, Rrn(Vtwlagf)                                                 
        From Votrea00/Vtwlagf                                                                       
        Where Lagcia = :Pcia And                                                                    
              Lagzna = :Pzona And                                                                   
              Lagcmp = :Pcamp And                                                                   
              Lagtpv In ('A','E') And                                                               
              Lagcto = :Lagcto                                                                      
        Order By Rrn(Vtwlagf) Desc;                                                                 
                                                                                                    
       Exec Sql                                                                                     
        Open Cur1;                                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cur1 Into :Lagcia, :Lagcdg, :Lagzna, :Rrn;                                            
                                                                                                    
       Dow Sqlcod <> 100 And Sqlcod >= *Zeros;                                                      
        If Hnro < Nro;                                                                              
        Exec Sql                                                                                    
         Select Rzgfic Into :RzgficW                                                                
         From Votrea00/Vtmrzgf                                                                      
         Where Rzgcia = :Pcia And                                                                   
               Rzgzna = :Lagzna;                                                                    
        If RzgficW <> *Blanks And %Char(%Date()) >= RzgficW;                                        
         Osec = *Blanks;                                                                            
         Exec Sql                                                                                   
          Select Gldsct Into :Osec                                                                  
          From Votrea00/Vtmgldf                                                                     
          Where Gldcia = :Pcia And Gldcte = :Lagcdg;                                                
         If Osec <> PrSectorContac(Lagcia:Lagcto);                                                  
          Oonro = *Zeros;                                                                           
          Exec Sql                                                                                  
           Select Count(0) Into :Oonro                                                              
           From Votrea00/Vtwlagf                                                                    
           Where Lagcia = :Pcia And                                                                 
                 Lagcto = :Lagcto And Rrn(Vtwlagf) = :Rrn;                                          
           If Oonro > *Zeros;                                                                       
            Exec Sql                                                                                
             Delete Votrea00/Vtwlagf                                                                
             Where Lagcia = :Pcia And Lagcto = :Lagcto And                                          
                   Rrn(Vtwlagf) = :Rrn;                                                             
          Endif;                                                                                    
         Endif;                                                                                     
        Else;                                                                                       
         Clear Oonro;                                                                               
         Exec Sql                                                                                   
          Select Count(0) Into :Oonro                                                               
          From Votrea00/Vtweagf                                                                     
          Where Eagcig = :Pcia And                                                                  
                Eagcdg = :Lagcdg And                                                                
                Eagzng = :Lagzna And                                                                
                Eagcco = :Pcia And                                                                  
                Eagcdc = :Lagcto And                                                                
                Eagznc = :Lagzna;                                                                   
          If Oonro <= *Zeros Or Nro > Oonro;                                                        
            Exec Sql                                                                                
             Insert Into Jortega/VtwlagfPro(LAGCIA,                                                 
                                            LAGCDG,                                                 
                                            LAGCMP,                                                 
                                            LAGZNA,                                                 
                                            LAGCCO,                                                 
                                            LAGCTO,                                                 
                                            LAGNRC,                                                 
                                            LAGEST,                                                 
                                            LAGESQ,                                                 
                                            LAGSUB,                                                 
                                            LAGTPV,                                                 
                                            LAGVLP,                                                 
                                            LAGNOR,                                                 
                                            LAGUSR,                                                 
                                            LAGFEC,                                                 
                                            LAGHOR,                                                 
                                            LAGETG)                                                 
              Select LAGCIA,                                                                        
                     LAGCDG,                                                                        
                     LAGCMP,                                                                        
                     LAGZNA,                                                                        
                     LAGCCO,                                                                        
                     LAGCTO,                                                                        
                     LAGNRC,                                                                        
                     LAGEST,                                                                        
                     LAGESQ,                                                                        
                     LAGSUB,                                                                        
                     LAGTPV,                                                                        
                     LAGVLP,                                                                        
                     LAGNOR,                                                                        
                     LAGUSR,                                                                        
                     LAGFEC,                                                                        
                     LAGHOR,                                                                        
                     LAGETG                                                                         
              From Votrea00/Vtwlagf                                                                 
              Where Lagcia = :Pcia And Lagcto = :Lagcto And                                         
                    Rrn(Vtwlagf) = :Rrn;                                                            
                                                                                                    
            Exec Sql                                                                                
             Delete Votrea00/Vtwlagf                                                                
             Where Lagcia = :Pcia And Lagcto = :Lagcto And                                          
                   Rrn(Vtwlagf) = :Rrn;                                                             
          Endif;                                                                                    
        Endif;                                                                                      
        Endif;                                                                                      
        Hnro += 1;                                                                                  
        Exec Sql                                                                                    
         Fetch Cur1 Into :Lagcia, :Lagcdg, :Lagzna, :Rrn;                                           
        Enddo;                                                                                      
                                                                                                    
       Exec Sql                                                                                     
        Close Cur1;                                                                                 
                                                                                                    
      /End-Free                                                                                     
     PPrValidar        E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Consulta el Sector del Contacto                //                       
       //------------------------------------------------------------------//                       
     PPrSectorContac   B                                                                            
     DPrSectorContac   Pi             1a                                                            
     DWcia                            3a                                                            
     DWcedula                         9  0                                                          
     ** Definiciòn de Variables de Trabajo                                                          
     DSqlStr           s           1000a   Inz(*Blanks)                                             
     DPsector          s              1a   Inz(*Blanks)                                             
      /Free                                                                                         
       SqlStr = *Blanks;                                                                            
       SqlStr = 'Select Trim(Mldm11) ' +                                                            
                'From Opcapf/Pmllst' + Pcia + ' ' +                                                 
                'Where MlcstÑ = ' + %Trim(%Editc(Wcedula:'Z'));                                     
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur2 From :SqlStr;                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor2 Cursor For Cur2;                                                            
                                                                                                    
       Exec Sql                                                                                     
        Open Cursor2;                                                                               
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor2 Into :Psector;                                                                
                                                                                                    
       If Sqlcod = 100 Or Sqlcod < *Zeros;                                                          
        Psector = *Blanks;                                                                          
       Endif;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close Cursor2;                                                                              
                                                                                                    
       If Osec = Psector And Psector <> *Blanks;                                                    
        Psector = *Blanks;                                                                          
       Endif;                                                                                       
                                                                                                    
       Return Psector;                                                                              
      /End-Free                                                                                     
     PPrSectorContac   E                                                                            
