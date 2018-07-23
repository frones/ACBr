{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 20/08/2004: Daniel Simoes de Almeida
|*  - Primeira Versao ACBrCHQ
******************************************************************************}

{$I ACBr.inc}

unit ACBrCHQClass;

interface
uses ACBrDevice, ACBrECF,
     Classes,
     {$IFDEF COMPILER6_UP} Types, {$ELSE} ACBrD5, Windows, {$ENDIF}
     Contnrs;

{ Classe para Modelo de Cheque, com definicao para o posicionamento dos campos}
type
TACBrCHQModelo = class
 private
    fsLinhaExtenso1: Integer;
    fsLinhaExtenso2: Integer;
    fsLinhaLocal: Integer;
    fsLinhaValor: Integer;
    fsLinhaFavorecido: Integer;
    fsColunaExtenso1: Integer;
    fsColunaExtenso2: Integer;
    fsColunaLocal: Integer;
    fsColunaMes: Integer;
    fsColunaValor: Integer;
    fsColunaDia: Integer;
    fsColunaFavorecido: Integer;
    fsColunaAno: Integer;
    fsBanco: String;

 public
    property Banco         : String  read fsBanco         write fsBanco ;
    property LinhaValor    : Integer read fsLinhaValor    write fsLinhaValor ;
    property LinhaExtenso1 : Integer read fsLinhaExtenso1 write fsLinhaExtenso1;
    property LinhaExtenso2 : Integer read fsLinhaExtenso2 write fsLinhaExtenso2;
    property LinhaFavorecido : Integer read fsLinhaFavorecido
       write fsLinhaFavorecido ;
    property LinhaLocal     : Integer read fsLinhaLocal    write fsLinhaLocal ;
    property ColunaValor    : Integer read fsColunaValor   write fsColunaValor ;
    property ColunaExtenso1 : Integer read fsColunaExtenso1
       write fsColunaExtenso1 ;
    property ColunaExtenso2 : Integer read fsColunaExtenso2
       write fsColunaExtenso2 ;
    property ColunaFavorecido : Integer read fsColunaFavorecido
       write fsColunaFavorecido ;
    property ColunaLocal    : Integer read fsColunaLocal   write fsColunaLocal ;
    property ColunaDia      : Integer read fsColunaDia     write fsColunaDia ;
    property ColunaMes      : Integer read fsColunaMes     write fsColunaMes ;
    property ColunaAno      : Integer read fsColunaAno     write fsColunaAno ;
end;

{ Lista de Objetos do tipo TACBrCHQModelo
  A Procedure CarregaBemaFiINI, consegue carregar a definicição de Cheques de
  um arquivo no formato do Arquivo INI usado pela Bematech. Caso nenhum arquivo
  seja especificado, ou o arquivo nao exista, os valores Defaults serao
  utilizados (Mais explicações no Implementação da CarregaBemaFiINI abaixo) }
TACBrCHQModelos = class(TObjectList)
  private
    fsArquivoBemaFiINI: String;
    procedure SetArquivoBemaFiINI(const Value: String);
    procedure CarregaBemaFiINI  ;

  protected
    procedure SetObject (Index: Integer; Item: TACBrCHQModelo);
    function GetObject (Index: Integer): TACBrCHQModelo;

  public
    constructor Create( AOwnsObjects : Boolean ) ;

    function AchaModeloBanco( Banco : String ) : TACBrCHQModelo ;
    Property ArquivoBemaFiINI : String read fsArquivoBemaFiINI
       write SetArquivoBemaFiINI ;

    function Add (Obj: TACBrCHQModelo): Integer;
    procedure Insert (Index: Integer; Obj: TACBrCHQModelo);
    property Objects [Index: Integer]: TACBrCHQModelo
      read GetObject write SetObject; default;
  end;

{ Classe generica de Cheques, nao implementa nenhum modelo especifico, apenas
  declara a Classe. NAO DEVE SER INSTANCIADA. Usada apenas como base para
  as demais Classes de Imp.Cheques como por exemplo a classe TACBrCHQChronos }
type

{ TACBrCHQClass }

TACBrCHQClass = class
  private
    procedure SetAtivo(const Value: Boolean);
    procedure SetfpBanco(const Value: String);
    procedure SetfpObservacao(const Value: String);

  protected
    fpDevice  : TACBrDevice ;
    fpECF     : TACBrECF ;
    fpModelosCheque : TACBrCHQModelos ;

    fpAtivo   : Boolean ;
    fpModeloStr: String;

    fpPaginaDeCodigo: Word;

    { comando enviado para a impressora }
    fpComandoEnviado  : String ;

    { resposta recebida da impressora }
    fpRespostaComando : String ;

    fpValor: Double;
    fpCidade: String;
    fpFavorecido: String;
    fpBanco: String;
    fpData: TDateTime;
    fpObservacao : String;
    fpCMC7 : String;
    fpBomPara : TDateTime;

    function GetChequePronto: Boolean; Virtual ;
    procedure SetfpData(const Value: TDateTime);
    procedure SetfpCidade(const Value: String);
    procedure SetfpFavorecido(const Value: String);
    procedure SetBomPara(const Value: TDateTime); virtual;

    procedure EnviarStr(AStr: string);
  public
    constructor Create(AOwner: TComponent);
    Destructor Destroy  ; override ;

    Property Ativo  : Boolean read fpAtivo write SetAtivo ;
    procedure Ativar ; virtual ;
    procedure Desativar ; virtual ;

    Property ECF : TACBrECF read fpECF write fpECF ;
    Property ModeloStr: String  read fpModeloStr ;

    // Pagina de Código
    property PaginaDeCodigo: Word   read fpPaginaDeCodigo write fpPaginaDeCodigo;

    property Banco      : String    read fpBanco      write SetfpBanco ;
    property Valor      : Double    read fpValor      write fpValor ;
    property Data       : TDateTime read fpData       write SetfpData ;
    property Cidade     : String    read fpCidade     write SetfpCidade ;
    property Favorecido : String    read fpFavorecido write SetfpFavorecido ;
    property Observacao : String    read fpObservacao write SetfpObservacao ;

    Property ComandoEnviado  : String    read fpComandoEnviado ;
    property RespostaComando : String    read fpRespostaComando;
    property CMC7            : String    read fpCMC7;
    property BomPara         : TDateTime read fpBomPara         write SetBomPara;

    property ChequePronto : Boolean read GetChequePronto ;

    procedure ImprimirLinha( AString : AnsiString ) ; Virtual ;
    procedure ImprimirVerso( AStringList : TStrings ) ; Virtual ;

    procedure ImprimirCheque ; Virtual ;
    Procedure TravarCheque ; Virtual ;
    Procedure DestravarCheque ; Virtual ;

    function CodificarPaginaDeCodigo(ATexto: String): AnsiString;
end ;


implementation
Uses ACBrCHQ, ACBrUtil,
     SysUtils, IniFiles ;

{ TACBrCHQClass }
constructor TACBrCHQClass.Create(AOwner: TComponent);
begin
  if not (AOwner is TACBrCHQ) then
     raise Exception.create(ACBrStr('Essa Classe deve ser instanciada por TACBrCHQ'));

  { Criando ponteiro interno para as Propriedade SERIAL de ACBrCHQ,
    para permitir as Classes Filhas o acesso a essas propriedades do Componente}

  fpDevice        := (AOwner as TACBrCHQ).Device ;
  fpECF           := (AOwner as TACBrCHQ).ECF ;
  fpModelosCheque := (AOwner as TACBrCHQ).ModelosCheque ;
  fpDevice.SetDefaultValues ;

  fpAtivo      := false ;
  fpModeloStr  := 'Não Definida' ;

  fpRespostaComando := '';
  fpComandoEnviado  := '';

  fpValor      := 0  ;
  fpCidade     := '' ;
  fpFavorecido := '' ;
  fpBanco      := '' ;
  fpData       := now;
  fpObservacao := '' ;
  fpCMC7       := '' ;

  // Pagina de Codigo
  fpPaginaDeCodigo := 0;   // 0 = Sem acentos
end;

destructor TACBrCHQClass.Destroy;
begin
  fpDevice := nil ; { Apenas remove referencia (ponteiros internos) }
  fpECF    := nil ;
  fpModelosCheque := nil ;
  
  inherited Destroy ;
end;

procedure TACBrCHQClass.SetAtivo(const Value: Boolean);
begin
  if Value then
     Ativar
  else
     Desativar ;
end;

procedure TACBrCHQClass.Ativar;
begin
  if fpAtivo then exit ;

  if fpDevice.Porta <> '' then
     fpDevice.Ativar ;

  fpAtivo := true ;
end;

procedure TACBrCHQClass.Desativar;
begin
  if not fpAtivo then exit ;

  if fpDevice.Porta <> '' then
     fpDevice.Desativar ;

  fpAtivo := false ;
end;

procedure TACBrCHQClass.ImprimirLinha( AString : AnsiString );
begin
  EnviarStr( CodificarPaginaDeCodigo(AString) + #13 + #10 );  { Adciona CR + LF }
end;

procedure TACBrCHQClass.ImprimirVerso(AStringList: TStrings);
Var A : Integer ;
begin
  TravarCheque ;

  For A := 0 to AStringList.Count - 1 do
     ImprimirLinha( StringOfChar(' ',10) + AStringList[A] );

  DestravarCheque ;
end;


procedure TACBrCHQClass.TravarCheque;
begin
 { Nada a fazer, apenas declara a Procudere para evitar Erros se ela nao for
   implementada na Classe filha }
end;

procedure TACBrCHQClass.DestravarCheque;
begin
 { Nada a fazer, apenas declara a Procudere para evitar Erros se ela nao for
   implementada na Classe filha }
end;

procedure TACBrCHQClass.ImprimirCheque;
begin
 { Nada a fazer, apenas declara a Procudere para evitar Erros se ela nao for
   implementada na Classe filha }
end;

function TACBrCHQClass.GetChequePronto: Boolean;
begin
  Result := true ;
end;


procedure TACBrCHQClass.SetfpBanco(const Value: String);
Var Val : Integer ;
begin
  if Value = fpBanco then exit ;

  if Value = '' then
     fpBanco := Value
  else
   begin
     Val := StrToIntDef( Value, 0) ;
     if (Val = 0) or (Val > 999) then
        raise Exception.Create(ACBrStr('Valor do Banco deve ser de 001 a 999'));

     fpBanco := IntToStrZero( Val, 3) ;
   end ;
end;

procedure TACBrCHQClass.SetfpData(const Value: TDateTime);
begin
   fpData := Value;
end;

procedure TACBrCHQClass.SetfpCidade(const Value: String);
begin
  fpCidade := CodificarPaginaDeCodigo( Trim(Value) );
end;

procedure TACBrCHQClass.SetfpFavorecido(const Value: String);
begin
  fpFavorecido := CodificarPaginaDeCodigo( Trim(Value) );
end;

procedure TACBrCHQClass.SetfpObservacao(const Value: String);
begin
  fpObservacao := CodificarPaginaDeCodigo( Value );
end;

procedure TACBrCHQClass.SetBomPara(const Value: TDateTime);
begin
  fpBomPara := Value;
end;

function TACBrCHQClass.CodificarPaginaDeCodigo(ATexto: String): AnsiString;
begin
  if fpPaginaDeCodigo > 0 then
     Result := TranslateString( ACBrStrToAnsi( ATexto ), fpPaginaDeCodigo )
  else
     Result := TiraAcentos( ATexto );
end ;

procedure TACBrCHQClass.EnviarStr(AStr: string);
begin
  try
    fpDevice.EnviaString(AStr);
  except
  end;

  Sleep(100);
end;

{ -------------------------------- TACBrCHQModelos --------------------------- }
constructor TACBrCHQModelos.Create(AOwnsObjects : Boolean);
begin
  inherited Create( AOwnsObjects );

  fsArquivoBemaFiINI := '' ;
end;

procedure TACBrCHQModelos.SetArquivoBemaFiINI(const Value: String);
Var OldValue : String ;
begin
  if fsArquivoBemaFiINI = Value then exit ;

  if not FileExists( Value ) then
     raise Exception.CreateFmt(ACBrStr('Arquivo %s não encontrado.'),[Value]);

  OldValue := fsArquivoBemaFiINI ;
  fsArquivoBemaFiINI := Value;
  try
     CarregaBemaFiINI ;
  except
     fsArquivoBemaFiINI := OldValue;
     raise ;
  end ;
end;

procedure TACBrCHQModelos.CarregaBemaFiINI ;
Var ArqIni : TMemIniFile ;
    ErroMsg: String ;
    Posicoes : TStringList ;
    Modelo : TACBrCHQModelo ;
    ArqTemp, L : String ;
    A,P : Integer ;
    wLinhaExtenso1, wLinhaExtenso2, wLinhaLocal, wLinhaValor, wColunaExtenso1,
    wColunaExtenso2, wColunaLocal, wLinhaFavorecido, wColunaMes, wColunaValor,
    wColunaDia, wColunaFavorecido, wColunaAno : Integer;
    wBanco: String;
begin
  try
     { Verificando se o arquivo é válido }
     if (fsArquivoBemaFiINI <> '') and (not FileExists( fsArquivoBemaFiINI )) then
     begin
        ErroMsg := 'Arquivo não encontrado.' ;
        fsArquivoBemaFiINI := '' ;
     end ;

     if fsArquivoBemaFiINI = '' then
        ArqTemp := ExtractFilePath( ParamStr(0) )+'BemaFi32.ini'
     else
        ArqTemp := fsArquivoBemaFiINI ;

     Clear ;
     ArqIni := TMemIniFile.Create( ArqTemp ) ;
     try
        { Adcionando valores default }
        if (not FileExists( ArqTemp )) then
        with ArqIni do begin
   {    linha da cidade/data ------------------------------------------------+
         linha do favorecido ---------------------------------------------+  |
          linha do extenso 2 ------------------------------------------+  |  |
          linha do extenso 1 ---------------------------------------+  |  |  |
     linha do valor numerico ------------------------------------+  |  |  |  |
               coluna do ano ---------------------------------+  |  |  |  |  |
               coluna do mes ------------------------------+  |  |  |  |  |  |
               coluna do dia ---------------------------+  |  |  |  |  |  |  |
            coluna da cidade ------------------------+  |  |  |  |  |  |  |  |
           coluna favorecido ---------------------+  |  |  |  |  |  |  |  |  |
            coluna extenso 2 ------------------+  |  |  |  |  |  |  |  |  |  |
            coluna extenso 1 ---------------+  |  |  |  |  |  |  |  |  |  |  |
    coluna do valor numerico ------------+  |  |  |  |  |  |  |  |  |  |  |  |
                                         |  |  |  |  |  |  |  |  |  |  |  |  |}
           WriteString('Formato','000','51,04,01,05,06,60,65,81,01,06,08,11,14') ;
           WriteString('Formato','001','51,10,01,06,18,50,54,71,02,05,08,10,12') ;
           WriteString('Formato','003','49,08,01,05,18,52,55,72,01,05,07,09,12') ;
           WriteString('Formato','004','52,09,01,05,18,50,53,72,02,06,09,11,13') ;
           WriteString('Formato','006','56,10,01,05,15,43,48,72,01,06,08,10,13') ;
           WriteString('Formato','008','56,17,01,07,18,50,55,71,03,06,09,11,13') ;
           WriteString('Formato','021','52,12,01,04,18,49,53,71,02,07,09,11,13') ;
           WriteString('Formato','022','52,07,01,04,15,44,49,71,02,06,08,10,13') ;
           WriteString('Formato','024','51,07,01,05,18,48,52,72,01,05,07,09,12') ;
           WriteString('Formato','027','52,12,01,06,18,45,55,71,01,05,08,10,12') ;
           WriteString('Formato','028','55,06,01,05,18,50,53,71,01,05,08,10,12') ;
           WriteString('Formato','029','55,12,01,04,18,50,55,72,01,06,08,10,13') ;
           WriteString('Formato','031','55,13,01,04,18,45,49,69,01,05,08,10,12') ;
           WriteString('Formato','032','56,14,01,04,18,45,49,71,02,05,07,09,12') ;
           WriteString('Formato','033','48,17,01,06,18,46,50,71,02,06,08,11,13') ;
           WriteString('Formato','034','49,14,01,04,15,45,57,71,01,05,07,09,11') ;
           WriteString('Formato','035','59,07,01,06,18,52,56,72,02,06,08,10,13') ;
           WriteString('Formato','036','58,11,01,05,18,50,53,72,02,06,08,10,12') ;
           WriteString('Formato','037','58,08,01,05,18,51,54,72,02,06,08,10,12') ;
           WriteString('Formato','038','56,10,01,04,18,51,56,72,02,07,10,12,14') ;
           WriteString('Formato','039','49,24,01,04,18,45,56,70,01,05,07,09,11') ;
           WriteString('Formato','041','56,09,01,04,18,54,61,72,03,07,09,12,14') ;
           WriteString('Formato','047','52,08,01,05,18,47,50,72,01,05,07,10,12') ;
           WriteString('Formato','048','54,12,01,04,18,45,49,68,02,05,08,10,13') ;
           WriteString('Formato','059','50,15,01,05,18,55,59,72,01,05,07,09,11') ;
           WriteString('Formato','070','54,05,01,05,18,48,52,72,02,06,08,10,12') ;
           WriteString('Formato','104','56,13,01,04,18,48,53,72,01,04,07,10,12') ;
           WriteString('Formato','106','52,12,01,05,18,52,55,71,02,07,09,11,13') ;
           WriteString('Formato','151','54,06,01,04,18,47,52,71,01,05,07,10,12') ;
           WriteString('Formato','153','51,09,01,05,18,51,55,72,01,05,08,10,13') ;
           WriteString('Formato','168','53,05,01,05,18,54,57,71,02,06,08,11,13') ;
           WriteString('Formato','200','52,06,01,05,18,47,52,71,01,05,07,10,12') ;
           WriteString('Formato','201','52,11,01,04,18,47,51,71,01,05,07,09,11') ;
           WriteString('Formato','206','56,14,01,06,18,53,56,72,01,06,08,10,13') ;
           WriteString('Formato','207','50,04,01,05,18,48,52,71,02,06,08,11,13') ;
           WriteString('Formato','211','48,11,01,05,18,52,56,71,03,07,09,12,14') ;
           WriteString('Formato','215','55,06,01,05,18,51,54,71,02,05,08,10,13') ;
           WriteString('Formato','220','56,09,01,05,18,49,53,71,02,05,08,10,12') ;
           WriteString('Formato','230','50,12,01,05,18,54,58,71,02,05,08,10,13') ;
           WriteString('Formato','231','52,12,01,05,18,53,58,72,02,06,08,10,12') ;
           WriteString('Formato','237','50,01,01,04,18,50,54,71,02,06,09,11,14') ;
           WriteString('Formato','244','48,14,01,04,18,49,53,71,03,06,09,11,13') ;
           WriteString('Formato','254','51,09,01,05,18,53,56,71,01,05,08,11,14') ;
           WriteString('Formato','275','51,07,01,04,18,46,52,68,03,08,10,12,14') ;
           WriteString('Formato','282','56,12,01,05,18,50,54,71,02,06,08,10,13') ;
           WriteString('Formato','291','52,12,01,05,18,47,49,71,02,06,08,10,12') ;
           WriteString('Formato','294','50,05,01,05,18,54,56,71,02,05,07,10,12') ;
           WriteString('Formato','302','51,07,01,05,18,47,51,71,02,06,08,10,13') ;
           WriteString('Formato','308','57,11,01,06,18,47,50,72,02,06,08,10,12') ;
           WriteString('Formato','320','54,06,01,04,18,48,51,72,02,05,08,10,13') ;
           WriteString('Formato','334','54,06,01,04,18,54,57,71,02,06,08,10,12') ;
           WriteString('Formato','341','54,08,01,05,18,50,54,72,02,06,09,12,15') ;
           WriteString('Formato','346','54,12,01,05,18,54,57,71,02,05,08,10,12') ;
           WriteString('Formato','347','53,15,01,04,18,47,51,72,02,06,09,11,14') ;
           WriteString('Formato','351','52,14,01,05,18,55,58,72,01,05,07,10,12') ;
           WriteString('Formato','353','52,07,01,05,18,53,58,71,02,05,07,10,12') ;
           WriteString('Formato','356','52,11,01,04,18,45,49,71,01,05,07,10,12') ;
           WriteString('Formato','369','47,07,01,05,18,51,55,71,02,06,08,10,12') ;
           WriteString('Formato','370','52,06,01,05,18,47,50,71,01,05,07,10,12') ;
           WriteString('Formato','372','51,07,01,04,18,46,49,71,02,06,08,11,13') ;
           WriteString('Formato','376','54,07,01,04,18,54,58,72,02,06,08,10,12') ;
           WriteString('Formato','388','46,09,01,06,18,48,52,72,02,06,09,11,14') ;
           WriteString('Formato','389','52,06,01,05,18,53,58,72,02,07,09,12,14') ;
           WriteString('Formato','392','49,12,01,05,18,54,58,72,02,05,07,11,13') ;
           WriteString('Formato','394','51,05,01,05,18,51,55,71,01,05,07,09,13') ;
           WriteString('Formato','399','54,12,01,04,18,52,57,72,01,05,07,10,12') ;
           WriteString('Formato','409','55,12,01,04,23,52,58,71,04,07,09,11,13') ;
           WriteString('Formato','415','54,12,01,06,18,50,54,72,03,07,10,12,14') ;
           WriteString('Formato','420','54,08,01,04,18,50,54,72,02,06,08,10,13') ;
           WriteString('Formato','422','58,17,03,07,18,52,58,72,03,06,08,11,13') ;
           WriteString('Formato','424','58,12,01,04,18,50,55,71,02,06,09,11,13') ;
           WriteString('Formato','434','56,08,01,05,18,50,54,72,02,06,09,11,13') ;
           WriteString('Formato','453','54,12,01,05,18,51,56,72,03,07,10,12,14') ;
           WriteString('Formato','456','48,11,01,05,18,47,50,71,02,06,08,10,12') ;
           WriteString('Formato','464','51,16,01,05,18,56,58,72,02,06,09,11,13') ;
           WriteString('Formato','472','53,12,01,05,18,50,53,71,02,06,09,10,14') ;
           WriteString('Formato','477','55,08,01,05,18,52,57,72,03,07,09,11,14') ;
           WriteString('Formato','479','53,07,01,05,18,50,53,71,02,06,08,10,12') ;
           WriteString('Formato','483','52,08,01,05,18,47,50,71,02,05,07,09,11') ;
           WriteString('Formato','487','58,17,01,05,18,48,52,72,02,06,08,11,13') ;
           WriteString('Formato','494','51,09,01,05,18,50,53,71,02,06,08,10,13') ;
           WriteString('Formato','602','56,10,01,03,18,47,52,66,02,05,07,10,13') ;
           WriteString('Formato','603','52,05,01,02,18,51,56,72,03,08,10,13,17') ;
           WriteString('Formato','607','51,09,01,05,18,53,56,72,02,05,08,10,12') ;
           WriteString('Formato','610','55,15,01,05,18,53,58,71,01,06,08,10,12') ;
           WriteString('Formato','630','49,05,01,05,18,47,52,71,01,06,08,10,13') ;
           WriteString('Formato','718','51,07,01,05,18,48,53,71,01,06,08,10,13') ;
           WriteString('Formato','756','51,10,01,06,18,50,54,71,02,05,08,10,12') ;
           WriteString('Formato','995','44,10,01,03,32,30,30,25,20,04,06,09,10') ;
           WriteString('Formato','996','61,10,01,03,57,30,30,25,16,04,06,09,02') ;
        end ;

        Posicoes := TStringList.Create ;
        try
           ArqIni.ReadSectionValues('Formato',Posicoes) ;
           For A := 0 to Posicoes.Count - 1 do
           begin
              L := StringReplace(Posicoes[A],'|',',',[rfReplaceAll]) ;
              { Linha válida ? }
              if (CountStr(L,'=') <> 1) or (CountStr(L,',') <> 12) then
                Continue ;

              try
                 { Banco }
                 P := pos('=',L) ;
                 wBanco := trim(copy(L,1,P-1)) ;
                 L := trim(copy(L,P+1,255)) ;
                 { ColunaValor }
                 P := pos(',',L) ;
                 wColunaValor := StrToInt(copy(L,1,P-1)) ;
                 L := trim(copy(L,P+1,255)) ;
                 { ColunaExtenso1 }
                 P := pos(',',L) ;
                 wColunaExtenso1 := StrToInt(copy(L,1,P-1)) ;
                 L := trim(copy(L,P+1,255)) ;
                 { ColunaExtenso2 }
                 P := pos(',',L) ;
                 wColunaExtenso2 := StrToInt(copy(L,1,P-1)) ;
                 L := trim(copy(L,P+1,255)) ;
                 { ColunaFavorecido }
                 P := pos(',',L) ;
                 wColunaFavorecido := StrToInt(copy(L,1,P-1)) ;
                 L := trim(copy(L,P+1,255)) ;
                 { ColunaLocal }
                 P := pos(',',L) ;
                 wColunaLocal := StrToInt(copy(L,1,P-1)) ;
                 L := trim(copy(L,P+1,255)) ;
                 { ColunaDia }
                 P := pos(',',L) ;
                 wColunaDia := StrToInt(copy(L,1,P-1)) ;
                 L := trim(copy(L,P+1,255)) ;
                 { ColunaMes }
                 P := pos(',',L) ;
                 wColunaMes := StrToInt(copy(L,1,P-1)) ;
                 L := trim(copy(L,P+1,255)) ;
                 { ColunaAno }
                 P := pos(',',L) ;
                 wColunaAno := StrToInt(copy(L,1,P-1)) ;
                 L := trim(copy(L,P+1,255)) ;

                 { LinhaValor }
                 P := pos(',',L) ;
                 wLinhaValor := StrToInt(copy(L,1,P-1)) ;
                 L := trim(copy(L,P+1,255)) ;
                 { LinhaExtenso1 }
                 P := pos(',',L) ;
                 wLinhaExtenso1 := StrToInt(copy(L,1,P-1)) ;
                 L := trim(copy(L,P+1,255)) ;
                 { LinhaExtenso2 }
                 P := pos(',',L) ;
                 wLinhaExtenso2 := StrToInt(copy(L,1,P-1)) ;
                 L := trim(copy(L,P+1,255)) ;
                 { LinhaFavorecido }
                 P := pos(',',L) ;
                 wLinhaFavorecido := StrToInt(copy(L,1,P-1)) ;
                 L := trim(copy(L,P+1,255)) ;
                 { LinhaLocal }
                 P := pos(' ',L) ;
                 if P = 0 then
                    P := length(L)+1 ;
                 wLinhaLocal := StrToInt(copy(L,1,P-1)) ;

                 Modelo := TACBrCHQModelo.Create ;
                 with Modelo do
                 begin
                    Banco           := wBanco ;
                    LinhaValor      := wLinhaValor ;
                    LinhaExtenso1   := wLinhaExtenso1 ;
                    LinhaExtenso2   := wLinhaExtenso2 ;
                    LinhaFavorecido := wLinhaFavorecido ;
                    LinhaLocal      := wLinhaLocal ;
                    ColunaValor     := wColunaValor ;
                    ColunaExtenso1  := wColunaExtenso1 ;
                    ColunaExtenso2  := wColunaExtenso2 ;
                    ColunaFavorecido:= wColunaFavorecido ;
                    ColunaLocal     := wColunaLocal ;
                    ColunaDia       := wColunaDia ;
                    ColunaMes       := wColunaMes ;
                    ColunaAno       := wColunaAno ;
                 end ;

                 Add( Modelo ) ;
              except
              end ;
           end ;
        finally
           Posicoes.Free ;
        end ;
     finally
        ArqIni.Free ;
     end ;

     if (Count = 0) and (FileExists( ArqTemp )) then
     begin
        ErroMsg := 'Formatação do arquivo não está correta '+sLineBreak+
                'Use o mesmo formato do arquivo BemaFI32.INI da Bematech ' ;

        if fsArquivoBemaFiINI <> '' then  { Tenta ler valores padroes }
        begin
           fsArquivoBemaFiINI := '' ;
           CarregaBemaFiINI ;     { Chamada Recursiva }
        end ;
     end ;
  finally
     if ErroMsg <> '' then
        raise Exception.Create(ACBrStr('Erro lendo arquivo: '+ArqTemp + sLineBreak +
                                ErroMsg + sLineBreak +
                                '(Valores padrões serão utilizados.)') ) ;
  end ;
end;

function TACBrCHQModelos.AchaModeloBanco(Banco: String): TACBrCHQModelo;
Var A : Integer ;
begin
  if Count = 0 then
     CarregaBemaFiINI ;

  Result := nil ;
  Banco  := IntToStrZero( StrToInt(Banco),3) ;

  For A := 0 to Count -1 do
     if Objects[A].Banco = Banco then
     begin
        Result := Objects[A] ;
        Break ;
      end ;

  if Result = nil then
     if Banco <> '000' then
        Result := AchaModeloBanco('000') ; { chamada recursiva, achar padrao }

end;

function TACBrCHQModelos.Add(Obj: TACBrCHQModelo): Integer;
begin
  Result := inherited Add(Obj) ;
end;

function TACBrCHQModelos.GetObject(Index: Integer): TACBrCHQModelo;
begin
  Result := inherited GetItem(Index) as TACBrCHQModelo ;
end;

procedure TACBrCHQModelos.Insert(Index: Integer; Obj: TACBrCHQModelo);
begin
  inherited Insert(Index, Obj);
end;

procedure TACBrCHQModelos.SetObject(Index: Integer; Item: TACBrCHQModelo);
begin
  inherited SetItem (Index, Item) ;
end;

end.
