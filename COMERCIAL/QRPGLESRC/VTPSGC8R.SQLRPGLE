      ***********************************************************************                       
      ** Programa      : VTPSGC8R                                          **                       
      ** Descripciòn   : PGM que Inactiva Lideres Vtmnpif                  **                       
      ** Autor         : (JOrtega)                                         **                       
      ** Fecha Creación: 05 de Marzo de 2021                               **                       
      ***********************************************************************                       
      ** Definición de Directivas de Compilación                                                    
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
                                                                                                    
      ** Definición de Variables de Trabajo                                                         
     DNro              s              9  0 Inz(*Zeros)                                              
     DElements         s              5I 0 Inz(%Elem(DsArrayCampos))                                
     DNum              s              9  0 Inz(*Zeros)                                              
     DIndex0           s              5U 0                                                          
     DNpiciaX          s              3                                                             
     DNpicdgX          s              9  0                                                          
                                                                                                    
     ** Definiciòn de Prototipos de Trabajo                                                         
     DDsArrayCampos    Ds                  Dim(1000) Qualified                                      
     DNpicia                          3a                                                            
     DNpicdg                          9  0                                                          
                                                                                                    
       //------------------------------------------------------------------//                       
       //              P r o g r a m a   P r i n c i p a l                 //                       
       //------------------------------------------------------------------//                       
      /Free                                                                                         
       Clear DsArrayCampos;                                                                         
       Exec Sql                                                                                     
        Declare Cur0 Cursor For                                                                     
        Select Npicia, Npicdg                                                                       
        From Votrea00/Vtmnpif                                                                       
        Where Npiest = ' '                                                                          
        Group By Npicia, Npicdg                                                                     
        Having Count(0) > 1                                                                         
        Order By Npicia;                                                                            
                                                                                                    
       Exec Sql                                                                                     
        Open Cur0;                                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Fetch From Cur0 For :Elements Rows Into :DsArrayCampos;                                     
                                                                                                    
       Num = SqlEr3;                                                                                
                                                                                                    
       Exec Sql                                                                                     
        Close Cur0;                                                                                 
                                                                                                    
       For Index0 = 1 To Num By 1;                                                                  
        Clear NpiciaX;                                                                              
        Clear NpicdgX;                                                                              
        NpiciaX = DsArrayCampos(Index0).Npicia;                                                     
        NpicdgX = DsArrayCampos(Index0).Npicdg;                                                     
        PrDetalle(NpiciaX:NpicdgX);                                                                 
       EndFor;                                                                                      
                                                                                                    
       *InLr = *On;                                                                                 
      /End-Free                                                                                     
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Consulta el Detalle de Lideres Activas         //                       
       //------------------------------------------------------------------//                       
     PPrDetalle        B                                                                            
     DPrDetalle        Pi                                                                           
     DNpiciaW                         3                                                             
     DNpicdgW                         9  0                                                          
     ** Definiciòn de Variables de Trabajo                                                          
     DEmail            s            200a   Inz(*Blanks)                                             
     DWcomercial       s           1000a   Inz(*Blanks)                                             
     DNpicia           s              3                                                             
     DNpicdg           s              9  0                                                          
     DNpizna           s              3    Inz(*Blanks)                                             
     DNpisec           s              1    Inz(*Blanks)                                             
      /Free                                                                                         
       Exec Sql                                                                                     
        Declare Cur1 Cursor For                                                                     
        Select Npicia, Npicdg, Npizna, Npisec                                                       
        From Votrea00/Vtmnpif                                                                       
        Where Npiest = ' ' And Npicia = :NpiciaW And Npicdg = :NpicdgW;                             
                                                                                                    
       Exec Sql                                                                                     
        Open Cur1;                                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cur1 Into :Npicia, :Npicdg, :Npizna, :Npisec;                                         
                                                                                                    
       Dow SqlCod <> 100 And SqlCod >= *Zeros;                                                      
        Clear Nro;                                                                                  
        Exec Sql                                                                                    
         Select Count(0) Into :Nro                                                                  
         From Votrea00/Vtmgldf                                                                      
         Where Gldcia = :Npicia And                                                                 
               Gldcte = :Npicdg And                                                                 
               Gldzon = :Npizna And                                                                 
               Gldsct = :Npisec And                                                                 
               Gldest = ' ';                                                                        
         If Nro <= 0;                                                                               
          Exec Sql                                                                                  
           Update Votrea00/Vtmnpif Set Npiest = 'D'                                                 
           Where Npicia = :Npicia And                                                               
                 Npicdg = :Npicdg And                                                               
                 Npizna = :Npizna And                                                               
                 Npisec = :Npisec And                                                               
                 Npiest = ' ';                                                                      
         Endif;                                                                                     
                                                                                                    
        Exec Sql                                                                                    
         Fetch Cur1 Into :Npicia, :Npicdg, :Npizna, :Npisec;                                        
       Enddo;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close Cur1;                                                                                 
      /End-Free                                                                                     
     PPrDetalle        E                                                                            
