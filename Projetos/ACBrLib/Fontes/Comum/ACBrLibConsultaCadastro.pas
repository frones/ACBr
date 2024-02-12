{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Rafael Teno Dias                                }
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

unit ACBrLibConsultaCadastro;

interface

uses
  Classes, SysUtils, contnrs,
  ACBrDFeComum.RetConsCad,
  ACBrLibResposta;

type
  { TConsultaCadastroItemResposta }
  TConsultaCadastroItemResposta = class(TACBrLibRespostaBase)
  private
    Farquivo: string;
    FCEP: string;
    FcMun: Integer;
    FCNAE: Integer;
    FcSit: Integer;
    FdBaixa: TDateTime;
    FdIniAtiv: TDateTime;
    FdUltSit: TDateTime;
    FIE: string;
    FCNPJ: string;
    FCPF: string;
    FIEAtual: String;
    FIEUnica: String;
    Fnro: String;
    FUF: String;
    FxBairro: String;
    FxCpl: String;
    FxFant: String;
    FxLgr: String;
    FxMun: String;
    FxNome: String;
    FxRegApur: String;

  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const infCad: TInfCadCollectionItem);

  published
    property arquivo: String read Farquivo write Farquivo;
    property IE: string read FIE write FIE;
    property CNPJ: String read FCNPJ write FCNPJ;
    property CPF: String read FCPF write FCPF;
    property UF: String read FUF write FUF;
    property cSit: Integer read FcSit write FcSit;
    property xNome: String read FxNome write FxNome;
    property xFant: String read FxFant write FxFant;
    property xRegApur: String read FxRegApur write FxRegApur;
    property CNAE: Integer read FCNAE write FCNAE;
    property dIniAtiv: TDateTime read FdIniAtiv write FdIniAtiv;
    property dUltSit: TDateTime read FdUltSit write FdUltSit;
    property dBaixa: TDateTime read FdBaixa write FdBaixa;
    property IEUnica: String read FIEUnica write FIEUnica;
    property IEAtual: String read FIEAtual write FIEAtual;
    property xLgr: String read FxLgr write FxLgr;
    property nro: String read Fnro write Fnro;
    property xCpl: String read FxCpl write FxCpl;
    property xBairro: String read FxBairro write FxBairro;
    property cMun: Integer read FcMun write FcMun;
    property xMun: String read FxMun write FxMun;
    property CEP: String read FCEP write FCEP;

  end;

  { TConsultaCadastroResposta }
  TConsultaCadastroResposta = class(TACBrLibRespostaBase)
  private
    FMsg: string;
    Fversao: string;
    FtpAmb: string;
    FverAplic: string;
    FcStat: integer;
    FxMotivo: string;
    FcUF: integer;
    FdhRecbto: TDateTime;
    FIE: string;
    FCNPJ: string;
    FCPF: string;
    FUF: string;
    FdhCons: TDateTime;
    FItems: TObjectList;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
    destructor Destroy; override;

    procedure Processar(const retConsCad: TRetConsCad);

  published
    property Msg: string read FMsg write FMsg;
    property Versao: string read Fversao write Fversao;
    property tpAmb: string read FtpAmb write FtpAmb;
    property VerAplic: string read FverAplic write FverAplic;
    property CStat: integer read FcStat write FcStat;
    property XMotivo: string read FxMotivo write FxMotivo;
    property CUF: integer read FcUF write FcUF;
    property DhRecbto: TDateTime read FdhRecbto write FdhRecbto;
    property IE: string read FIE write FIE;
    property CNPJ: string read FCNPJ write FCNPJ;
    property CPF: string read FCPF write FCPF;
    property UF: string read FUF write FUF;
    property dhCons: TDateTime read FdhCons write FdhCons;
    property Items: TObjectList read FItems;

  end;

implementation

uses
  ACBrUtil.Base, ACBrUtil.FilesIO, ACBrUtil.Strings,
  ACBrLibConsts;

{ TConsultaCadastroItemResposta }
constructor TConsultaCadastroItemResposta.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

procedure TConsultaCadastroItemResposta.Processar(const infCad: TInfCadCollectionItem);
begin
  FIE := infCad.IE;
  FCNPJ := infCad.CNPJ;
  FCPF := infCad.CPF;
  FUF := infCad.UF;
  FcSit := infCad.cSit;
  FxNome := infCad.xNome;
  FxFant := infCad.xFant;
  FxRegApur := infCad.xRegApur;
  FCNAE := infCad.CNAE;
  FdIniAtiv := infCad.dIniAtiv;
  FdUltSit := infCad.dUltSit;
  FdBaixa := infCad.dBaixa;
  FIEUnica := infCad.IEUnica;
  FIEAtual := infCad.IEAtual;
  FxLgr := infCad.xLgr;
  Fnro := infCad.nro;
  FxCpl := infCad.xCpl;
  FxBairro := infCad.xBairro;
  FcMun := infCad.cMun;
  FxMun := infCad.xMun;
  FCEP := IntToStrZero(infCad.CEP,8);
end;

{ TConsultaCadastroResposta }
constructor TConsultaCadastroResposta.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespConsultaCadastro, ATipo, AFormato);

  FItems := TObjectList.Create(True);
end;

destructor TConsultaCadastroResposta.Destroy;
begin
  FItems.Clear;
  FItems.Free;

  inherited Destroy;
end;

procedure TConsultaCadastroResposta.Processar(const retConsCad: TRetConsCad);
var
  i: Integer;
  Item: TConsultaCadastroItemResposta;
begin
  FVersao := RetConsCad.Versao;
  FVerAplic := RetConsCad.VerAplic;
  FCStat := RetConsCad.cStat;
  FXMotivo := RetConsCad.XMotivo;
  FCUF := RetConsCad.cUF;
  FdhCons := RetConsCad.dhCons;
  FIE  := RetConsCad.IE;
  FCNPJ := RetConsCad.CNPJ;
  FCPF := RetConsCad.CPF;
  FCPF := RetConsCad.CPF;
  FUF := REtConsCad.UF;

  FItems.Clear;

  with retConsCad do
  begin
    for i := 0 to InfCad.Count - 1 do
    begin
      Item := TConsultaCadastroItemResposta.Create('INFCAD' + Trim(IntToStrZero(i + 1, 3)), Tipo, Formato);
      Item.Processar(InfCad.Items[i]);
      FItems.Add(Item);
    end;
  end;

end;

end.

