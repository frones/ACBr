{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Willian Hübner e Elton Barbosa (EMBarbosa)      }
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
unit UACBrLCDPR;

interface

uses
  ACBrBase,
  SysUtils, Classes,
  Registro0000, Registro0010, Registro0030, Bloco0040, Bloco0050, BlocoQ,
  Registro9999, LCDPRUtils, LCDPRBlocos, uDadosContador;

type
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrLCDPR = class(TACBrComponent)
  private
    FConteudo : TStringList;
    FBloco0000: TRegistro0000;
    FBloco0010: TRegistro0010;
    FBloco9999: TRegistro9999;
    FBloco0040: TBlocos0040;
    FBloco0050: TBloco0050;
    FBlocoQ: TBlocoQ;
    FBloco0030: TRegistro0030;
    FPath: String;
    FDelimitador: String;
    FReplaceDelimitador: Boolean;
    FArquivo: String;
    FDadosContador: TContador;

    procedure SetBloco0000(const Value: TRegistro0000);
    procedure SetBloco0010(const Value: TRegistro0010);
    procedure SetBloco0030(const Value: TRegistro0030);
    procedure SetBloco0040(const Value: TBlocos0040);
    procedure SetBloco0050(const Value: TBloco0050);
    procedure SetBloco9999(const Value: TRegistro9999);
    procedure SetBlocoQ(const Value: TBlocoQ);
    procedure SetArquivo(const Value: String);
    procedure SetDelimitador(const Value: String);
    procedure SetReplaceDelimitador(const Value: Boolean);
    procedure SetPath(const Value: String);
    procedure SetDadosContador(const Value: TContador);

    function GetArquivo : String;

    function AddCampo(const Value : String; AddDelimiter: Boolean = True ) : String;

    procedure WriteBloco0000;
    procedure WriteBloco0010;
    procedure WriteBloco0030;
    procedure WriteBloco0040;
    procedure WriteBloco0050;
    procedure WriteBlocoQ100;
    procedure WriteBloco9999;
  public
    constructor Create(AOwner: TComponent); override; /// Create
    destructor Destroy; override; /// Destroy
    ///
    procedure PrepararArquivo;
    procedure GerarBlocos;
    procedure SalvarBlocos;
    procedure limparRegistros;

    property Arquivo : String read GetArquivo write SetArquivo;
  published
    property Path : String read FPath write SetPath;
    property Delimitador : String read FDelimitador write SetDelimitador;
    property ReplaceDelimitador: Boolean read FReplaceDelimitador write SetReplaceDelimitador;
    property Bloco0000 : TRegistro0000 read FBloco0000 write SetBloco0000;
    property Bloco0010 : TRegistro0010 read FBloco0010 write SetBloco0010;
    property Bloco0030 : TRegistro0030 read FBloco0030 write SetBloco0030;
    property Bloco0040 : TBlocos0040 read FBloco0040 write SetBloco0040;
    property Bloco0050 : TBloco0050 read FBloco0050 write SetBloco0050;
    property BlocoQ : TBlocoQ read FBlocoQ write SetBlocoQ;
    property Bloco9999 : TRegistro9999 read FBloco9999 write SetBloco9999;
    property DadosContador : TContador read FDadosContador write SetDadosContador;
  end;

implementation

uses
  ACBrUtil.Strings;

{ TLCDPR }

function TACBrLCDPR.AddCampo(const Value: String; AddDelimiter: Boolean): String;
begin
  Result := Trim(Value);
  if AddDelimiter then
     Result := Result + Delimitador;
end;

constructor TACBrLCDPR.Create(AOwner: TComponent);
begin
  inherited;

  FBloco0000          := TRegistro0000.Create;
  FBloco0010          := TRegistro0010.Create;
  FBloco0030          := TRegistro0030.Create;
  FBloco0040          := TBlocos0040.Create;
  FBloco0050          := TBloco0050.Create;
  FBlocoQ             := TBlocoQ.Create;
  FBloco9999          := TRegistro9999.Create;
  FDadosContador      := TContador.Create;
  FConteudo           := TStringList.Create;
  FArquivo            := 'LCDPR';

  Delimitador         := '|';
  ReplaceDelimitador  := False;
end;

destructor TACBrLCDPR.Destroy;
begin
  FBloco0000.Free;
  FBloco0010.Free;
  FBloco0030.Free;
  FBloco0040.Free;
  FBloco0050.Free;
  FBlocoQ.Free;
  FBloco9999.Free;
  FDadosContador.Free;
  FConteudo.Free;
  inherited;
end;

procedure TACBrLCDPR.GerarBlocos;
begin
  WriteBloco0000;
  WriteBloco0010;
  WriteBloco0030;
  WriteBloco0040;
  WriteBloco0050;
  WriteBlocoQ100;
  WriteBloco9999;
end;

function TACBrLCDPR.GetArquivo: String;
var
  diaInicial, mesInicial, anoInicial: Word;
  diaFinal, mesFinal, anoFinal: Word;
  dataInicial, dataFinal: String;
begin
  DecodeDate(FBloco0000.DT_INI, anoInicial, mesInicial, diaInicial);
  DecodeDate(FBloco0000.DT_FIN, anoFinal, mesFinal, diaFinal);
  dataInicial := FormatFloat('0#', diaInicial) + '/' + FormatFloat('0#', mesInicial) + '/' + FormatFloat('####', anoInicial);
  dataFinal := FormatFloat('0#', diaFinal) + '/' + FormatFloat('0#', mesFinal) + '/' + FormatFloat('####', anoFinal);

  Result := FArquivo + '_' + FBloco0000.NOME + '_' +
      OnlyNumber(dataInicial) + '_' + OnlyNumber(dataFinal) +
      '.txt';
end;

procedure TACBrLCDPR.PrepararArquivo;
begin
  if (Trim(FArquivo) = '') or (Trim(FPath) = '') then
    raise Exception.Create('Caminho ou nome do arquivo não informado!');

  FConteudo.Clear;
end;

procedure TACBrLCDPR.SalvarBlocos;
begin
  FConteudo.SaveToFile(Path + Arquivo);
end;

procedure TACBrLCDPR.SetArquivo(const Value: String);
begin
  if Value = '' then
    raise Exception.Create('Campo não pode ser vazio!');

  FArquivo := Value;
end;

procedure TACBrLCDPR.SetBloco0000(const Value: TRegistro0000);
begin
  FBloco0000 := Value;
end;

procedure TACBrLCDPR.SetBloco0010(const Value: TRegistro0010);
begin
  FBloco0010 := Value;
end;

procedure TACBrLCDPR.SetBloco0030(const Value: TRegistro0030);
begin
  FBloco0030 := Value;
end;

procedure TACBrLCDPR.SetBloco0040(const Value: TBlocos0040);
begin
  FBloco0040 := Value;
end;

procedure TACBrLCDPR.SetBloco0050(const Value: TBloco0050);
begin
  FBloco0050 := Value;
end;

procedure TACBrLCDPR.SetBloco9999(const Value: TRegistro9999);
begin
  FBloco9999 := Value;
end;

procedure TACBrLCDPR.SetBlocoQ(const Value: TBlocoQ);
begin
  FBlocoQ := Value;
end;

procedure TACBrLCDPR.SetDadosContador(const Value: TContador);
begin
  FDadosContador := Value;
end;

procedure TACBrLCDPR.SetDelimitador(const Value: String);
begin
  if Value = '' then
    raise Exception.Create('Campo não pode ser vazio!');

  FDelimitador := Value;
end;

procedure TACBrLCDPR.SetPath(const Value: String);
begin
  if Value = '' then
    raise Exception.Create('Campo não pode ser vazio!');

  FPath := Value;
end;

procedure TACBrLCDPR.SetReplaceDelimitador(const Value: Boolean);
begin
  FReplaceDelimitador := Value;
end;

procedure TACBrLCDPR.WriteBloco0000;
begin
  with Bloco0000 do
    begin
      FConteudo.Add(
        AddCampo('0000') +
        AddCampo('LCDPR') +
        AddCampo(CodVerToStr(COD_VER)) +
        AddCampo(OnlyNumber(CPF)) +
        AddCampo(NOME) +
        AddCampo(IndInicioToStr(IND_SIT_INI_PER)) +
        AddCampo(IndSitEspToStr(SIT_ESPECIAL)) +
        AddCampo(formatDate(DT_SIT_ESP)) +
        AddCampo(formatDate(DT_INI)) +
        AddCampo(formatDate(DT_FIN), False)
      );
    end;
end;

procedure TACBrLCDPR.WriteBloco0010;
begin
  with Bloco0010 do
    begin
      FConteudo.Add (
        AddCampo('0010') +
        AddCampo(IndFormaApurToStr(FORMA_APUR), False)
      );
    end;
end;

procedure TACBrLCDPR.WriteBloco0030;
begin
  with Bloco0030 do
    begin
      FConteudo.Add(
        AddCampo('0030') +
        AddCampo(ENDERECO) +
        AddCampo(NUM) +
        AddCampo(COMPL) +
        AddCampo(BAIRRO) +
        AddCampo(UF) +
        AddCampo(COD_MUN) +
        AddCampo(CEP) +
        AddCampo(NUM_TEL) +
        AddCampo(EMAIL, False)
      );
    end;
end;

procedure TACBrLCDPR.WriteBloco0040;
var
  i,j : Integer;
begin
  for I := 0 to Pred(Bloco0040.Blocos.Count) do
    begin
      with Bloco0040.Blocos[i].Bloco0040 do
        begin
          FConteudo.Add (
            AddCampo('0040') +
            AddCampo(Format('%.3d',[(COD_IMOVEL)])) +
            AddCampo(PAIS) +
            AddCampo(MOEDA) +
            AddCampo(IntToStr(CAD_ITR)) +
            AddCampo(CAEPF) +
            AddCampo(INSCR_ESTADUAL) +
            AddCampo(NOME_IMOVEL) +
            AddCampo(ENDERECO) +
            AddCampo(NUM) +
            AddCampo(COMPL) +
            AddCampo(BAIRRO) +
            AddCampo(UF) +
            AddCampo(COD_MUN) +
            AddCampo(CEP) +
            AddCampo(TipoExploracaoToStr(TIPO_EXPLORACAO)) +
            AddCampo(formatNumeric(PARTICIPACAO, 5), False)
          );
        end;

      for j := 0 to Pred(Bloco0040.Blocos[i].Bloco0045.Count) do
        begin
          with Bloco0040.Blocos[i].Bloco0045[j] do
            begin
              FConteudo.Add (
                AddCampo('0045') +
                AddCampo(Format('%.3d',[(Bloco0040.Blocos[i].Bloco0040.COD_IMOVEL)])) +
                AddCampo(TipoContraparteToStr(TIPO_CONTRAPARTE)) +
                AddCampo(ID_CONTRAPARTE) +
                AddCampo(NOME_CONTRAPARTE) +
                AddCampo(formatNumeric(PERC_CONTRAPARTE, 5), False)
              );
            end;
        end;
    end;
end;

procedure TACBrLCDPR.WriteBloco0050;
var
  i : Integer;
begin
  for i := 0 to Pred(FBloco0050.CONTAS.Count) do
    begin
      with FBloco0050.CONTAS[i] do
        begin
          FConteudo.Add (
            AddCampo('0050') +
            AddCampo(Format('%.3d', [(COD_CONTA)])) +   // "Código da conta bancária (do Registro 0050)  Caso tenha sido pago em espécie ""000""  caso utilize numerário em trânsito ""999"""
            AddCampo(PAIS_CTA) +
            AddCampo(Format('%.3d', [(BANCO)])) +
            AddCampo(NOME_BANCO) +
            AddCampo(AGENCIA) +
            AddCampo(NUM_CONTA, False)
          );
        end;
    end;
end;

procedure TACBrLCDPR.WriteBloco9999;
begin
  with FDadosContador do
    begin
      FConteudo.Add (
        AddCampo('9999') +
        AddCampo(IDENT_NOME) +
        AddCampo(IDENT_CPF_CNPJ) +
        AddCampo(IND_CRC) +
        AddCampo(EMAIL) +
        AddCampo(FONE) +
        AddCampo(IntToStr(FConteudo.Count + 1), False)
      );
    end;
end;

procedure TACBrLCDPR.WriteBlocoQ100;
var
  i : Integer;
begin
  for i := 0 to Pred(FBlocoQ.RegistrosQ100.Count) do
    begin
      with FBlocoQ.RegistrosQ100[i] do
        begin
          FConteudo.Add(
            AddCampo('Q100') +
            AddCampo(formatDate(DATA)) +
            AddCampo(Format('%.3d',[(COD_IMOVEL)])) +
            AddCampo(Format('%.3d',[(COD_CONTA)])) +
            AddCampo(NUM_DOC) +
            AddCampo(TipoDocToStr(TIPO_DOC)) +
            AddCampo(HISTORICO) +
            AddCampo(ID_PARTIC) +
            AddCampo(TipoLancToStr(TIPO_LANC)) +
            AddCampo(formatNumeric(VL_ENTRADA)) +
            AddCampo(formatNumeric(VL_SAIDA)) +
            AddCampo(formatNumeric(SLD_FIN)) +
            AddCampo(NAT_SLD_FIN, False)
          );
        end;
    end;

  for i := 0 to Pred(FBlocoQ.RegistrosQ200.Count) do
    begin
      with FBlocoQ.RegistrosQ200[i] do
        begin
          FConteudo.Add(
            AddCampo('Q200') +
            AddCampo(MES) +
            AddCampo(formatNumeric(VL_ENTRADA)) +
            AddCampo(formatNumeric(VL_SAIDA)) +
            AddCampo(formatNumeric(SLD_FIN)) +
            AddCampo(NAT_SLD_FIN, False)
          );
        end;
    end;
end;

procedure TACBrLCDPR.limparRegistros;
begin
  Bloco0040.Blocos.Clear;
  Bloco0050.CONTAS.Clear;
  BlocoQ.RegistrosQ100.Clear;
  BlocoQ.RegistrosQ200.Clear;

  Bloco9999.QTD_LIN := 0;
end;

end.
