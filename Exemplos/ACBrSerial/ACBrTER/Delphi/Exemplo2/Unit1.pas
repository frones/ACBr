unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ACBrBase, ACBrTER , ACBrDevice, ExtCtrls, Spin, jpeg,
  ComCtrls , IniFiles, DB, ADODB ;

type
  TFrmPrincipal = class(TForm)
    ACBrTER1: TACBrTER;
    PageControl: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Panel2: TPanel;
    ListBoxFluxoTeclas: TListBox;
    GroupBox1: TGroupBox;
    Label4: TLabel;
    cbxModelo: TComboBox;
    Label1: TLabel;
    cbxPorta: TComboBox;
    ckbComutadora: TCheckBox;
    ckbRotacao: TCheckBox;
    edtPassoRotacao: TEdit;
    lblPassoRotacao: TLabel;
    Label2: TLabel;
    cbxBaudRate: TComboBox;
    Label3: TLabel;
    edtIntervalo: TEdit;
    Label5: TLabel;
    cbxTerminais: TSpinEdit;
    btnSalvar: TButton;
    BancoDados: TADOConnection;
    QryCadProdutos: TADOQuery;
    QryComandas: TADOQuery;
    SPItensComanda: TADOStoredProc;
    procedure ACBrTER1RecebeChar(Terminal: Word; Char: Char);
    procedure FormShow(Sender: TObject);
    procedure btnSalvarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  TeclaRecebida   : integer ;
  Comando         : integer ;
  Diretorio       : string ;
  
  MODELO          : string ;
  PORTA           : string ;
  BAUDRATE        : string ;
  INTERVALO       : string ;
  NUMEROTERMINAIS : integer ;
  COMUTADORA      : string ;
  ROTACAO         : string ;
  ROTACAOPASSO    : string ;
  procedure LeConfiguracoes ;
  procedure IniciaArquivosTemp ;

    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmPrincipal: TFrmPrincipal;

implementation

{$R *.dfm}




procedure TFrmPrincipal.ACBrTER1RecebeChar(Terminal: Word; Char: Char);
  var
  i                  : integer ;
  NomeArquivo        : string ;   // Referente ao Numero de cada Terminal
  Arquivo            : TextFile ;
  SR                 : TSearchRec ;
  Lista              : TStringList ;
  ComandoExecutado   : String ;
  ArmazenaTecla      : String ;
  ComandoFinalizador : String ;
  ArmazenaTerminal   : integer ;


  procedure EsvaziaBufferTeclas ;
   var i : integer ;
     begin
     for i := 0 to ListBoxFluxoTeclas.Items.Count-1 do
         begin
         ArmazenaTerminal := strtoint(copy(ListBoxFluxoTeclas.Items.Strings[i],1,pos('=',ListBoxFluxoTeclas.Items.Strings[i])-1)) ;
         if ArmazenaTerminal = Terminal then
            begin
            ListBoxFluxoTeclas.Items.Strings[i] := ''  ;
            ListBoxFluxoTeclas.Items.Strings[i] := vartostr(Terminal) + '='  ;
            end ;
         end ;
     end ;     

  procedure ContaComandos ;
     begin
     Lista := TStringList.Create ;
     try
     Lista.LoadFromFile( Diretorio +'\'+ NomeArquivo );
     Comando := Lista.Count ;
     finally
        Lista.Free;
       end;
     end;

  procedure CadastraComando ;
   var i : integer ;
       ValorArmazenagem : String ;
       ErroQtd : integer ;
     begin
     for i := 0 to ListBoxFluxoTeclas.Items.Count-1 do
         begin
         ArmazenaTerminal := strtoint(copy(ListBoxFluxoTeclas.Items.Strings[i],1,pos('=',ListBoxFluxoTeclas.Items.Strings[i])-1)) ;
         if ArmazenaTerminal = Terminal then
            begin
            ValorArmazenagem := trim(copy(ListBoxFluxoTeclas.Items.Strings[i],pos('=',ListBoxFluxoTeclas.Items.Strings[i])+1,50)) ;

            // Inicio pesquisando se a comanda esta cadastrada e ativa no sistema
            if Comando = 0 then
               begin
               QryComandas.close ;
               QryComandas.SQL.Clear ;
               QryComandas.SQL.Add('Select * from CadastroComandas where CodigoBarras=:@CodigoBarras');
               QryComandas.Parameters.ParamByName('@CodigoBarras').Value := ValorArmazenagem  ;
               QryComandas.Open ;
               if QryComandas.FieldByName('CodigoBarras').AsString = '' then
                  begin
                  EsvaziaBufferTeclas ;
                  ACBrTER1.LimpaTela( Terminal );
                  ACBrTER1.EnviaString( 'Comanda inval !' , Terminal );
                  ACBrTER1.PosicionaCursor(2,1, Terminal );
                  sleep(1500) ;
                  ACBrTER1.LimpaTela( Terminal );
                  ACBrTER1.EnviaString( 'Comanda' , Terminal );
                  ACBrTER1.PosicionaCursor(2,1, Terminal );
                  abort ;
                  end ;
               end ;
            // Fim pesquisando se a comanda esta cadastrada e ativa no sistema


            // Inicio pesquisando o produto no cadastro de produtos no Banco de Dados
            if Comando = 1 then
               begin
               if pos('.',ValorArmazenagem) > 0 then
                  begin
                  EsvaziaBufferTeclas ;
                  ACBrTER1.LimpaTela( Terminal );
                  ACBrTER1.EnviaString( 'Prod.invalido !' , Terminal );
                  ACBrTER1.PosicionaCursor(2,1, Terminal );
                  sleep(1500) ;
                  ACBrTER1.LimpaTela( Terminal );
                  ACBrTER1.EnviaString( 'Codigo Produto' , Terminal );
                  ACBrTER1.PosicionaCursor(2,1, Terminal );
                  abort ;
                  end ;
               if pos(',',ValorArmazenagem) > 0 then
                  begin
                  EsvaziaBufferTeclas ;
                  ACBrTER1.LimpaTela( Terminal );
                  ACBrTER1.EnviaString( 'Prod.invalido!' , Terminal );
                  ACBrTER1.PosicionaCursor(2,1, Terminal );
                  sleep(1500) ;
                  ACBrTER1.LimpaTela( Terminal );
                  ACBrTER1.EnviaString( 'Codigo Produto' , Terminal );
                  ACBrTER1.PosicionaCursor(2,1, Terminal );
                  abort ;
                  end ;
               QryCadProdutos.close ;
               QryCadProdutos.SQL.Clear ;
               QryCadProdutos.SQL.Add('Select * from CadProdutos where CodigoBarras=:@CodigoBarras');
               QryCadProdutos.Parameters.ParamByName('@CodigoBarras').Value := formatfloat('0000000000000',strtofloat(ValorArmazenagem)) ;
               QryCadProdutos.Open ;
               if QryCadProdutos.FieldByName('Codigo').AsString = '' then
                  begin
                  EsvaziaBufferTeclas ;
                  ACBrTER1.LimpaTela( Terminal );
                  ACBrTER1.EnviaString( 'Prod.invalido !' , Terminal );
                  ACBrTER1.PosicionaCursor(2,1, Terminal );
                  sleep(1500) ;
                  ACBrTER1.LimpaTela( Terminal );
                  ACBrTER1.EnviaString( 'Codigo Produto' , Terminal );
                  ACBrTER1.PosicionaCursor(2,1, Terminal );
                  abort ;
                  end
                   else
                    begin
                    ACBrTER1.LimpaTela( Terminal );
                    ACBrTER1.EnviaString( copy(QryCadProdutos.FieldByName('DescricaoRedusida').AsString,1,16) , Terminal );
                    ACBrTER1.PosicionaCursor(2,1, Terminal );
                    sleep(1000) ;
                    end ;
               end ;
            // Fim pesquisando o produto no cadastro de produtos no Banco de Dados

            // Inicio verificando digitação da quantidade
            if Comando = 2 then
               begin
               if pos('.',ValorArmazenagem) > 0 then
                  begin
                  EsvaziaBufferTeclas ;
                  ACBrTER1.LimpaTela( Terminal );
                  ACBrTER1.EnviaString( 'Qtd.invalida !' , Terminal );
                  ACBrTER1.PosicionaCursor(2,1, Terminal );
                  sleep(1500) ;
                  ACBrTER1.LimpaTela( Terminal );
                  ACBrTER1.EnviaString( 'Qtd Produto' , Terminal );
                  ACBrTER1.PosicionaCursor(2,1, Terminal );
                  abort ;
                  end ;
               if pos(',,',ValorArmazenagem) > 0 then
                  begin
                  EsvaziaBufferTeclas ;
                  ACBrTER1.LimpaTela( Terminal );
                  ACBrTER1.EnviaString( 'Qtd.invalida !' , Terminal );
                  ACBrTER1.PosicionaCursor(2,1, Terminal );
                  sleep(1500) ;
                  ACBrTER1.LimpaTela( Terminal );
                  ACBrTER1.EnviaString( 'Qtd Produto' , Terminal );
                  ACBrTER1.PosicionaCursor(2,1, Terminal );
                  abort ;
                  end ;
               // Trata quando o usuario digita algo antes de chamar o peso
               if length(ValorArmazenagem) >= 5 then
                  begin
                  ErroQtd := length(ValorArmazenagem) - 5 ;
                  delete(ValorArmazenagem , 1 ,ErroQtd);
                  insert(',',ValorArmazenagem,3);
                  end ;
               // Fim : Tratamento quando o usuario digita algo antes de chamar o peso



               end ;
            // Fim verificando digitação da quantidade



            ComandoExecutado := ComandoExecutado + ValorArmazenagem ;
            NomeArquivo := Diretorio +'\'+ NomeArquivo ;
            AssignFile(Arquivo, NomeArquivo);
            Append(Arquivo);
            WriteLN(Arquivo , ComandoExecutado );
            CloseFile(Arquivo) ;
            end ;
         end ;
     end;

  procedure VerificaComandoFinalizador ;
   var i : integer ;
     begin
     for i := 0 to ListBoxFluxoTeclas.Items.Count-1 do
         begin
         ArmazenaTerminal := strtoint(copy(ListBoxFluxoTeclas.Items.Strings[i],1,pos('=',ListBoxFluxoTeclas.Items.Strings[i])-1)) ;
         if ArmazenaTerminal = Terminal then
            ComandoFinalizador := trim(copy(ListBoxFluxoTeclas.Items.Strings[i],pos('=',ListBoxFluxoTeclas.Items.Strings[i])+1,50)) ;
         end ;
     end;        

  procedure ResetaArquivo ;
     begin
     AssignFile(Arquivo, Diretorio + '\' + NomeArquivo);
     Rewrite(Arquivo);
     CloseFile(Arquivo) ;
     ACBrTER1.LimpaTela( Terminal );
     ACBrTER1.EnviaString( 'Comanda' , Terminal );
     ACBrTER1.PosicionaCursor(2,1, Terminal );
     end;

  procedure ArmazenaBufferTeclas ;
   var i : integer ;
     begin
     for i := 0 to ListBoxFluxoTeclas.Items.Count-1 do
         begin
         ArmazenaTerminal := strtoint(copy(ListBoxFluxoTeclas.Items.Strings[i],1,pos('=',ListBoxFluxoTeclas.Items.Strings[i])-1)) ;
         if ArmazenaTerminal = Terminal then
            begin
            ArmazenaTecla := ListBoxFluxoTeclas.Items.Strings[i] ;
            ListBoxFluxoTeclas.Items.Strings[i] := ArmazenaTecla + char  ;
            end ;
         end ;
     end ;


   procedure GravaBancoDados ;
      var Linhas            : TStringList ;
          i                 : integer;
          ConteudoLinha     : string ; 
          Ficha             : string ;
          CodigoBarras      : string ;
          Descricao         : string ;
          Quantidade        : double ;
          ValorUnitario     : double ;
          ValorTotalItem    : double ;
          AliquotaICMS      : string ;
          DataHora          : TDateTime ;
     begin
     Linhas  := TStringList.Create;
     try
     Linhas.LoadFromFile( Diretorio +'\'+ NomeArquivo ); // Carregando Arquivo
     for i := 0 to Pred(Linhas.Count) do
         begin
         ConteudoLinha := trim(copy(Linhas.Strings[i],pos('=',Linhas.Strings[i])+1,50)) ;
         case i of
           0 : Ficha        := ConteudoLinha ;
           1 : CodigoBarras := ConteudoLinha ;
           2 : Quantidade   := strtofloat(ConteudoLinha) ;
           end ; // End Case
         end;
     DataHora := date + time ;
     QryCadProdutos.close ;
     QryCadProdutos.SQL.Clear ;
     QryCadProdutos.SQL.Add('Select * from CadProdutos where CodigoBarras=:@CodigoBarras');
     QryCadProdutos.Parameters.ParamByName('@CodigoBarras').Value := formatfloat('0000000000000',strtofloat(CodigoBarras)) ;
     QryCadProdutos.Open ;
     ValorUnitario  := QryCadProdutos.fieldbyname('PrecoVenda').AsFloat ;
     ValorTotalItem := Quantidade * QryCadProdutos.fieldbyname('PrecoVenda').AsFloat ;
     AliquotaICMS   := QryCadProdutos.fieldbyname('ICMSSaida').AsString ;
     SPItensComanda.Parameters.parambyname('@CODIGOFICHA').value             :=  Ficha ;
     SPItensComanda.Parameters.parambyname('@NUMEROCONTROLEITEM').value      :=  1 ;
     SPItensComanda.Parameters.parambyname('@MESA').value                    :=  'NENHUMA' ;
     SPItensComanda.Parameters.parambyname('@DATAHORACONTROLEPEDIDO').value  :=  DataHora ;
     SPItensComanda.Parameters.parambyname('@CODIGOBARRAS').value            :=  QryCadProdutos.fieldbyname('CodigoBarras').AsString ;
     SPItensComanda.Parameters.parambyname('@DESCRICAOPRODUTO').value        :=  QryCadProdutos.fieldbyname('Descricao').AsString ;
     SPItensComanda.Parameters.parambyname('@QUANTIDADE').value              :=  Quantidade ;
     SPItensComanda.Parameters.parambyname('@VALORUNITARIO').value           :=  ValorUnitario ;
     SPItensComanda.Parameters.parambyname('@VALORTOTAL').value              :=  ValorTotalItem ;
     SPItensComanda.Parameters.parambyname('@ALIQUOTAICMS').value            :=  AliquotaICMS ;
     SPItensComanda.Parameters.parambyname('@NOMEATENDENTE').value           :=  'WILBOR' ;
     SPItensComanda.Parameters.parambyname('@IMPRESSORA').value              :=  'NENHUMA' ;
     SPItensComanda.Prepared ;
     try
       SPItensComanda.ExecProc ;
       finally
       sleep(50);
       end;
     finally
     Linhas.Free;
     end;
end ;

begin
  // Exit no caso de retorno inválido
  if (char = #2) or (char = #3) then
     exit ;
  // Fim : Exit no caso de retorno inválido

  // Converte em caracter o char recebido
  TeclaRecebida := Ord(char) ;
  // Fim : Converte em caracter o char recebido

  // Inicio executando Delete no visor do terminal
  if TeclaRecebida = 127 then
     begin
     EsvaziaBufferTeclas ;
     ACBrTER1.PosicionaCursor(2,1,Terminal);
     ACBrTER1.EnviaString( '                ' , Terminal );
     ACBrTER1.PosicionaCursor(2,1,Terminal);
     exit ;
     end ;
  // Fim executando Delete no visor do terminal

  // Inicio executando BackSpace para leitura da balança (Programe como quiser em outra tecla)
  if TeclaRecebida = 8 then
     begin
     ACBrTER1.LeBalanca(Terminal);
     exit ;
     end ;
  // Fim executando BackSpace para leitura da balança  (Programe como quiser em outra tecla)

  // Inicio tratando informações
  i := FindFirst(Diretorio + '\*.txt', faAnyFile, SR);
  while i = 0 do
        begin
        if (SR.Attr and faDirectory) <> faDirectory then
            begin
            NomeArquivo := vartostr(Terminal)+'.txt' ;
            if ( NomeArquivo = SR.Name) then
               begin
               if TeclaRecebida = 13 then
                  begin
                  ContaComandos ; // Conta Comandos efetuados por terminal

                  Case Comando of
                       0  : begin
                            ComandoExecutado := 'Comanda=' ;
                            CadastraComando ;
                            ACBrTER1.LimpaTela( Terminal );
                            ACBrTER1.EnviaString( 'Codigo Produto' , Terminal );
                            ACBrTER1.PosicionaCursor(2,1,Terminal);
                            EsvaziaBufferTeclas ;
                            exit ;
                            end ;

                       1  : begin
                            ComandoExecutado := 'CodigoProduto=' ;
                            CadastraComando ;
                            ACBrTER1.LimpaTela( Terminal );
                            ACBrTER1.EnviaString( 'Qtd Produto' , Terminal );
                            ACBrTER1.PosicionaCursor(2,1,Terminal);
                            EsvaziaBufferTeclas ;
                            exit ;
                            end ;

                       2  : begin
                            ComandoExecutado := 'QtdProduto=' ;
                            CadastraComando ;
                            ACBrTER1.LimpaTela( Terminal );
                            ACBrTER1.EnviaString( 'Fim 1=SIM 2=NAO' , Terminal );
                            ACBrTER1.PosicionaCursor(2,1,Terminal);
                            EsvaziaBufferTeclas ;
                            exit ;
                            end ;

                       3  : begin
                            VerificaComandoFinalizador ;
                            if (ComandoFinalizador) = '1' then
                               begin
                               EsvaziaBufferTeclas ;
                               ACBrTER1.LimpaTela( Terminal );
                               ACBrTER1.EnviaString( 'Finalizando...' , Terminal );
                               ACBrTER1.PosicionaCursor(2,1,Terminal);
                               sleep(200);
                               GravaBancoDados ;    
                               ResetaArquivo ;
                               exit ;
                               end ;
                            if (ComandoFinalizador) = '2' then
                               begin
                               EsvaziaBufferTeclas ;
                               ACBrTER1.LimpaTela( Terminal );
                               ACBrTER1.EnviaString( 'Oper.Abortada...' , Terminal );
                               sleep(1500) ;
                               ResetaArquivo ;
                               exit ;
                               end ;       
                            ACBrTER1.LimpaTela( Terminal );
                            ACBrTER1.EnviaString( 'Comando invalido' , Terminal );
                            ACBrTER1.PosicionaCursor(2,1,Terminal);
                            sleep(1500) ;
                            ACBrTER1.LimpaTela( Terminal );
                            ACBrTER1.EnviaString( 'Fim 1=SIM 2=NAO' , Terminal );
                            ACBrTER1.PosicionaCursor(2,1,Terminal);
                            EsvaziaBufferTeclas ;
                            exit ;
                            end ;

                    end ; // End do Case
                  end ;
               end ;
            end ;
        i := FindNext(SR);
        end ;
  // Fim tratando informações

  // Inicio Enviando Char para o terminal
  ACBrTER1.EnviaString( char , Terminal );
  // Inicio Enviando Char para o terminal

  // Inicio armazenando a tecla do terminal correspondente
  ArmazenaBufferTeclas ;
  // Fim armazenando a tecla do terminal correspondente    
end;



procedure TFrmPrincipal.FormShow(Sender: TObject);
begin
   PageControl.ActivePageIndex := 0 ;
   LeConfiguracoes ;
   IniciaArquivosTemp ;
end;



procedure TFrmPrincipal.LeConfiguracoes ;
  var ini : tinifile ;
begin
   ini := Tinifile.Create(extractfilepath(application.ExeName)+'CatronisWilbor.ini');
   MODELO          := (ini.readstring('Config','MODELO','')) ;
   PORTA           := (ini.readstring('Config','PORTA','')) ;
   BAUDRATE        := (ini.readstring('Config','BAUDRATE','')) ;
   INTERVALO       := (ini.readstring('Config','INTERVALO','')) ;
   NUMEROTERMINAIS := strtoint(ini.readstring('Config','NUMEROTERMINAIS','')) ;
   COMUTADORA      := (ini.readString('Config','COMUTADORA','')) ;
   ROTACAO         := (ini.readstring('Config','ROTACAO','')) ;
   ROTACAOPASSO    := (ini.readstring('Config','ROTACAOPASSO','')) ;

   // Inicio ativando o Componente
   if ACBrTER1.Ativo then
      ACBrTER1.Desativar ;
   ACBrTER1.Porta       := PORTA ;
   ACBrTER1.Device.Baud := strtoint(BAUDRATE) ;
   ACBrTER1.Intervalo   := strtoint(INTERVALO) ;
   if MODELO = 'Wilbor' then
      ACBrTER1.Modelo := terWilbor ;
   if COMUTADORA = 'SIM' then
      ACBrTER1.Comutadora := true
      else
      ACBrTER1.Comutadora := false ;
   if ROTACAO = 'SIM' then
      ACBrTER1.Rotacao.Ativo := true
      else
      ACBrTER1.Rotacao.Ativo := false ;
   ACBrTER1.Rotacao.Passo := strtoint(ROTACAOPASSO) ;
   ACBrTER1.ativar ;
   // Fim ativando o Componente

   // Inicio preenchendo componentes com seu valores
   cbxModelo.Text     :=  MODELO ;
   cbxPorta.Text      :=  PORTA ;
   cbxBaudRate.Text   :=  BAUDRATE ;
   edtIntervalo.Text  :=  INTERVALO ;
   cbxTerminais.Value :=  NUMEROTERMINAIS ;
   if (ini.readString('Config','COMUTADORA','')) = 'SIM' then
      ckbComutadora.Checked := true
      else
      ckbComutadora.Checked := false ;
   if (ini.readstring('Config','ROTACAO',''))  = 'SIM' then
      ckbRotacao.Checked := true
      else
      ckbRotacao.Checked := false ;
   edtPassoRotacao.Text := ROTACAOPASSO ;
   // Fim preenchendo componentes com seu valores

   ini.Free;
 end ;



procedure TFrmPrincipal.btnSalvarClick(Sender: TObject);
  var ini : tinifile ;
begin
    ini:=Tinifile.Create(extractfilepath(application.ExeName)+'CatronisWilbor.ini');
    ini.WriteString('Config','MODELO',cbxModelo.Text);
    ini.WriteString('Config','PORTA',cbxPorta.Text);
    ini.WriteString('Config','BAUDRATE',cbxBaudRate.Text);
    ini.WriteString('Config','INTERVALO',edtIntervalo.Text);
    ini.WriteString('Config','NUMEROTERMINAIS',cbxTerminais.Text);
    if ckbComutadora.Checked = true then
       ini.WriteString('Config','COMUTADORA','SIM')
       else
       ini.WriteString('Config','COMUTADORA','NAO') ;   
    if ckbRotacao.Checked = true then
       ini.WriteString('Config','ROTACAO','SIM')
       else
       ini.WriteString('Config','ROTACAO','NAO') ;
    ini.WriteString('Config','ROTACAOPASSO',edtPassoRotacao.Text) ;  
    ini.Free;
    application.MessageBox('Dados salvos com sucesso !!!','I N F O R M A Ç Ã O',mb_ok+mb_iconinformation);
end;



procedure TFrmPrincipal.IniciaArquivosTemp ;
  var i : integer ;
  NomeArquivo : string ;   // Referente ao Numero de cada Terminal
  Arquivo     : TextFile ;
  SR          : TSearchRec ;
begin
  // Inicio da Criação Diretório Temp do fluxo de dados
  Diretorio := extractfilepath(application.ExeName)+'Temp' ;
  if not DirectoryExists( Diretorio ) then
     CreateDir( Diretorio ) ;
  // Fim da Criação Diretório Temp do fluxo de dados

  // Inicio Deletando os arquivos Temp
  i := FindFirst(Diretorio + '\*.*', faAnyFile, SR);
  while i = 0 do
        begin
        if (SR.Attr and faDirectory) <> faDirectory then
            DeleteFile(Diretorio + '\' + SR.Name) ;
        i := FindNext(SR);
        end ;
  // Fim Deletando os arquivos temp

  // Inicio criando arquivos Temp com o número de cada terminal
  for i := 0 to NUMEROTERMINAIS-1 do
      begin
      NomeArquivo := Diretorio + '\' + vartostr(i+1) + '.txt';
      AssignFile(Arquivo, NomeArquivo);
      ReWrite(Arquivo); 
      CloseFile(Arquivo) ;
      end;
  // Inicio criando arquivos Temp com o número de cada terminal

  // Iniciando Terminais
  for i := 0 to NUMEROTERMINAIS-1 do
      begin
      ACBrTER1.LimpaTela(i);
      ACBrTER1.EnviaString( 'Comanda' , i );
      ACBrTER1.PosicionaCursor(2,1,i);
      end ;
  // Iniciando Terminais

  // Iniciando ListBox de Teclas a serem armazenadas para os terminais
  for i := 1 to NUMEROTERMINAIS do
      ListBoxFluxoTeclas.Items.Append(vartostr(i)+'=');
  // Fim ListBox de Teclas a serem armazenadas para os terminais
end;




procedure TFrmPrincipal.FormCreate(Sender: TObject);
   var Ini         : TIniFile ;
       DATABASE    : string ;
       SERVER      : string ;
       USERNAME    : string ;
       PASSWORD    : string ;
begin
 // Inicio Conectando base de dados
 Ini := Tinifile.Create(extractfilepath(application.ExeName)+'CatronisWilbor.ini');
 try
   DATABASE  := Ini.ReadString( 'BD','DATABASE'  ,'') ;
   SERVER    := Ini.ReadString( 'BD','SERVER'  ,'') ;
   USERNAME  := Ini.ReadString( 'BD','USERNAME'  ,'') ;
   PASSWORD  := Ini.ReadString( 'BD','PASSWORD'  ,'') ;
   finally
   Ini.Free ;
   end;        
 BancoDados.Connected := false ;
 BancoDados.ConnectionString := '' ;
 BancoDados.ConnectionString := 'Provider=SQLOLEDB.1;'          +
                                'Password='+PASSWORD+';'        +
                                'Persist Security Info=True;'   +
                                'User ID='+USERNAME+';'         +
                                'Initial Catalog='+DATABASE+';' +
                                'Data Source='+SERVER+';' ;
 BancoDados.Connected := true ;
 // Fim Conectando base de dados


end ;

end.
