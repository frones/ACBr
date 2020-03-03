{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Isaque Pinheiro                                 }
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
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrPAF_N_Class;

interface

uses SysUtils, Classes, DateUtils, ACBrTXTClass,
  ACBrPAF_N;

type

  { TPAF_N }

  TPAF_N = class(TACBrTXTClass)
  private
    FRegistroN1: TRegistroN1;       /// FRegistroN1
    FRegistroN2: TRegistroN2;       /// FRegistroN2
    FRegistroN3: TRegistroN3List;   /// FRegistroN3
    FRegistroN9: TRegistroN9;       /// FRegistroN9
    FOwner     : TObject;

    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    constructor Create( AOwner: TComponent); /// Create
    destructor Destroy; override; /// Destroy
    procedure LimpaRegistros;

    procedure LerDadosArquivo(const APathArquivo: String);

    procedure WriteRegistroN1;
    procedure WriteRegistroN2;
    procedure WriteRegistroN3;
    procedure WriteRegistroN9;

    property RegistroN1: TRegistroN1 read FRegistroN1 write FRegistroN1;
    property RegistroN2: TRegistroN2 read FRegistroN2 write FRegistroN2;
    property RegistroN3: TRegistroN3List read FRegistroN3 write FRegistroN3;
    property RegistroN9: TRegistroN9 read FRegistroN9 write FRegistroN9;
  end;

implementation

uses ACBrTXTUtils, ACBrUtil, ACBrPAF;

{ TPAF_N }

constructor TPAF_N.Create(AOwner: TComponent);
begin
  inherited create;

  if not (AOwner is TACBrPAF) then
    raise Exception.Create('Dono de TPAF_N deve ser do tipo TACBrPAF');

  FOwner := AOwner;
  CriaRegistros;
end;

procedure TPAF_N.CriaRegistros;
begin
  FRegistroN1 := TRegistroN1.Create;
  FRegistroN2 := TRegistroN2.Create;
  FRegistroN3 := TRegistroN3List.Create;
  FRegistroN9 := TRegistroN9.Create;

  FRegistroN9.TOT_REG := 0;
end;

destructor TPAF_N.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TPAF_N.LerDadosArquivo(const APathArquivo: String);
var
  Arquivo: TStringList;
  I: Integer;
  Linha: String;
  IdLinha: String;
begin
  if not FileExists(APathArquivo) then
    raise Exception.Create( ACBrStr( AnsiString( Format(
       'Arquivo "%s" informado não existe.', [APathArquivo])) ) );

  // ler os dados de um arquivo já gravado
  Arquivo := TStringList.Create;
  try
    Arquivo.Clear;
    Arquivo.LoadFromFile(APathArquivo);

    Self.RegistroN3.Clear;
    for I := 0 to Arquivo.Count - 1 do
    begin
      Linha   := Arquivo.Strings[I];
      IdLinha := Copy(Linha, 1, 2);

      if IdLinha = 'N1' then
      begin
        Self.RegistroN1.CNPJ        := Trim(Copy(Linha, 03, 14));
        Self.RegistroN1.IE          := Trim(Copy(Linha, 17, 14));
        Self.RegistroN1.IM          := Trim(Copy(Linha, 31, 14));
        Self.RegistroN1.RAZAOSOCIAL := Trim(Copy(Linha, 45, 14));
      end
      else
      if IdLinha = 'N2' then
      begin
        Self.RegistroN2.LAUDO  := Trim(Copy(Linha, 03, 10));
        Self.RegistroN2.NOME   := Trim(Copy(Linha, 13, 50));
        Self.RegistroN2.VERSAO := Trim(Copy(Linha, 63, 10));
      end
      else
      if IdLinha = 'N3' then
      begin
        with Self.RegistroN3.New do
        begin
          NOME_ARQUIVO := Trim(Copy(Linha, 03, 50));
          MD5          := Trim(Copy(Linha, 53, 32));
        end;
      end;
    end;
  finally
    Arquivo.Free;
  end;
end;

procedure TPAF_N.LiberaRegistros;
begin
  FRegistroN1.Free;
  FRegistroN2.Free;
  FRegistroN3.Free;
  FRegistroN9.Free;
end;

procedure TPAF_N.LimpaRegistros;
begin
  /// Limpa os Registros
  LiberaRegistros;
  /// Recriar os Registros Limpos
  CriaRegistros;
end;

procedure TPAF_N.WriteRegistroN1;
begin
  if Assigned(FRegistroN1) then
    begin
    with FRegistroN1 do
      begin
      Check(funChecaCNPJ(CNPJ),
        '(N1) ESTABELECIMENTO: O CNPJ "%s" digitado é inválido!', [CNPJ]);
      Check(funChecaIE(IE, UF),
        '(N1) ESTABELECIMENTO: A Inscrição Estadual "%s" digitada é inválida!', [IE]);
      ///
      Add(LFill('N1') +
           LFill(CNPJ, 14) +
           RFill(UpperCase(IE), 14) +
           RFill(UpperCase(IM), 14) +
           RFill(UpperCase(TiraAcentos(RAZAOSOCIAL)), 50));
      end;
    end;
end;

procedure TPAF_N.WriteRegistroN2;
begin
  if Assigned(FRegistroN2) then
      begin
      with FRegistroN2 do
        begin
        Add( LFill('N2') +
             RFill(UpperCase(LAUDO), 10) +
             RFill(UpperCase(TiraAcentos(NOME)), 50) +
             RFill(UpperCase(VERSAO), 10));
        end;
      end;
end;

// função para comparação dos nomes de arquivo que serão utilizados para
// ordenar os registros N3
function CompararRegistroN3(ARegN3_1, ARegN3_2: Pointer): Integer;
begin
  Result := AnsiCompareText(
    AnsiUpperCase(TRegistroN3(ARegN3_1).NOME_ARQUIVO),
    AnsiUpperCase(TRegistroN3(ARegN3_2).NOME_ARQUIVO)
  );
end;

procedure TPAF_N.WriteRegistroN3;
var
  intFor: integer;
  NomeArquivoCompleto, ApplicationDir : String ;
begin
  ApplicationDir := ExtractFilePath( ParamStr(0) );

  if Assigned(FRegistroN3) then
  begin
    FRegistroN3.Sort(@CompararRegistroN3);

    for intFor := 0 to FRegistroN3.Count - 1 do
    begin
      with FRegistroN3.Items[intFor] do
      begin
        // Não informou o MD5 ? Vamos calcula-lo...
        if Trim(MD5) = '' then
        begin
          NomeArquivoCompleto := NOME_ARQUIVO ;
          if pos( PathDelim, NomeArquivoCompleto ) = 0 then  // Nao informou Path ?
             NomeArquivoCompleto := ApplicationDir + NOME_ARQUIVO;

          try
             MD5 := TACBrPAF(FOwner).GetACBrEAD.MD5FromFile( NomeArquivoCompleto );
          except
             { Ignora Provavel erro de arquivo não encontrado }
          end ;
        end ;

        Add(LFill('N3') +
            RFill( UpperCase( ExtractFileName( NOME_ARQUIVO ) ), 50) +
            LFill( UpperCase( MD5 ), 32));
      end;

      FRegistroN9.TOT_REG := FRegistroN9.TOT_REG +  1;
    end;
  end;
end;


procedure TPAF_N.WriteRegistroN9;
begin
  if Assigned(FRegistroN9) then
    begin
    with FRegistroN9 do
      begin
        Add(LFill('N9') +
            LFill(FRegistroN1.CNPJ, 14) +
            RFill(UpperCase(FRegistroN1.IE), 14) +
            LFill(TOT_REG, 6, 0));
      end;
    end;
end;

end.

