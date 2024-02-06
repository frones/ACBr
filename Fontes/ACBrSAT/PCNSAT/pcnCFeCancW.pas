{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
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

unit pcnCFeCancW;

interface

uses
  SysUtils, Classes,
  ACBrUtil.Strings,
  ACBrUtil.FilesIO,
  ACBrDFeConsts,
  ACBrCFeConsts,
  pcnAuxiliar, pcnConversao, pcnGerador, pcnCFeCanc;

type

  TGeradorOpcoes = class;

  { TCFeCancW }

  TCFeCancW = class(TPersistent)
  private
    FApenasTagsAplicacao: Boolean;
    FGerador: TGerador;
    FCFeCanc: TCFeCanc;
    FOpcoes: TGeradorOpcoes;
    procedure GerarInfCFe;
    procedure GerarIde;
    procedure GerarEmit;
    procedure GerarEmitEnderEmit;
    procedure GerarDest;
    procedure GerarTotal;
    procedure GerarInfAdic;
    procedure GerarInfAdicObsFisco;

  public
    constructor Create(AOwner: TCFeCanc);
    destructor Destroy; override;
    function GerarXml( ApenasTagsAplicacao: Boolean = false ): boolean;
    function ObterNomeArquivo: string;
  published
    property Gerador: TGerador read FGerador write FGerador;
    property CFeCanc: TCFeCanc read FCFeCanc write FCFeCanc;
    property Opcoes: TGeradorOpcoes read FOpcoes write FOpcoes;
  end;

  TGeradorOpcoes = class(TPersistent)
  private
    FAjustarTagNro: boolean;
    FGerarTagIPIparaNaoTributado: boolean;
    FGerarTXTSimultaneamente: boolean;
    FNormatizarMunicipios: boolean;
    FGerarTagAssinatura: TpcnTagAssinatura;
    FPathArquivoMunicipios: string;
    FValidarInscricoes: boolean;
    FValidarListaServicos: boolean;
  published
    property AjustarTagNro: boolean read FAjustarTagNro write FAjustarTagNro;
    property GerarTagIPIparaNaoTributado: boolean read FGerarTagIPIparaNaoTributado write FGerarTagIPIparaNaoTributado;
    property GerarTXTSimultaneamente: boolean read FGerarTXTSimultaneamente write FGerarTXTSimultaneamente;
    property NormatizarMunicipios: boolean read FNormatizarMunicipios write FNormatizarMunicipios;
    property GerarTagAssinatura: TpcnTagAssinatura read FGerarTagAssinatura write FGerarTagAssinatura;
    property PathArquivoMunicipios: string read FPathArquivoMunicipios write FPathArquivoMunicipios;
    property ValidarInscricoes: boolean read FValidarInscricoes write FValidarInscricoes;
    property ValidarListaServicos: boolean read FValidarListaServicos write FValidarListaServicos;
  end;

  ////////////////////////////////////////////////////////////////////////////////


implementation

{ TCFeCancW }

constructor TCFeCancW.Create(AOwner: TCFeCanc);
begin
  inherited Create;
  FCFeCanc := AOwner;
  FGerador := TGerador.Create;
  FGerador.FIgnorarTagNivel := '|?xml version|CFe xmlns|infCFe versao|obsCont|obsFisco|';
  FOpcoes := TGeradorOpcoes.Create;
  FOpcoes.FAjustarTagNro := True;
  FOpcoes.FGerarTXTSimultaneamente := False;
  FOpcoes.FGerarTagIPIparaNaoTributado := True;
  FOpcoes.FNormatizarMunicipios := False;
  FOpcoes.FGerarTagAssinatura := taSomenteSeAssinada;
  FOpcoes.FValidarInscricoes := False;
  FOpcoes.FValidarListaServicos := False;
end;

destructor TCFeCancW.Destroy;
begin
  FGerador.Free;
  FOpcoes.Free;
  inherited Destroy;
end;

procedure TCFeCancW.GerarInfCFe;
begin
  GerarIde;
  GerarEmit;
  GerarDest;
  GerarTotal;
  GerarInfAdic;
end;

procedure TCFeCancW.GerarIde;
begin
  Gerador.wGrupo('ide', 'B01');

  if not FApenasTagsAplicacao then
  begin
    Gerador.wCampo(tcInt, 'B02', 'cUF', 02, 02, 1, CFeCanc.ide.cUF, DSC_CUF);
    if not ValidarCodigoUF(CFeCanc.ide.cUF) then
      Gerador.wAlerta('B02', 'cUF', DSC_CUF, ERR_MSG_INVALIDO);

    Gerador.wCampo(tcStr, 'B03', 'cNF      ', 06, 06, 1, IntToStrZero(CFeCanc.ide.cNF, 6), DSC_CDF);
    Gerador.wCampo(tcInt, 'B04', 'mod      ', 02, 02, 1, CFeCanc.ide.modelo, DSC_MOD);
    Gerador.wCampo(tcInt, 'B05', 'nserieSAT', 09, 09, 1, CFeCanc.ide.nserieSAT, DSC_SERIE);
    Gerador.wCampo(tcInt, 'B06', 'nCFe     ', 06, 06, 1, IntToStrZero(CFeCanc.ide.nCFe,6), DSC_NCFE);
    Gerador.wCampo(tcDatCFe, 'B07', 'dEmi  ', 10, 10, 1, CFeCanc.ide.dEmi, DSC_DEMI);
    Gerador.wCampo(tcHorCFe, 'B08', 'hEmi  ', 08, 08, 0, CFeCanc.ide.hEmi, DSC_HEMI);
    Gerador.wCampo(tcInt, 'B09', 'cDV      ', 01, 01, 1, CFeCanc.Ide.cDV, DSC_CDV);
  end;

  Gerador.wCampoCNPJCPF('B10', 'B10', CFeCanc.Ide.CNPJ);
  Gerador.wCampo(tcStr, 'B11', 'signAC',344, 344, 1, CFeCanc.Ide.signAC, DSC_SIGNAC);

  if not FApenasTagsAplicacao then
    Gerador.wCampo(tcStr, 'B12', 'assinaturaQRCODE', 441, 441, 1, CFeCanc.Ide.assinaturaQRCODE, DSC_QRCODE);

  Gerador.wCampo(tcInt, 'B13', 'numeroCaixa', 03, 03, 1, CFeCanc.ide.numeroCaixa, DSC_NUMEROCAIXA);
  Gerador.wGrupo('/ide');
end;

procedure TCFeCancW.GerarEmit;
var
  InscEst: String;
begin
  Gerador.wGrupo('emit', 'C01');

  if not FApenasTagsAplicacao then
  begin
    Gerador.wCampoCNPJCPF('C02', 'C02', CFeCanc.Emit.CNPJ);
    Gerador.wCampo(tcStr, 'C03', 'xNome', 01, 60, 1, CFeCanc.Emit.xNome, DSC_XNOME);
    Gerador.wCampo(tcStr, 'C04', 'xFant', 01, 60, 0, CFeCanc.Emit.xFant, DSC_XNOME);

    (**)GerarEmitEnderEmit;

    InscEst := Trim(OnlyNumber(CFeCanc.Emit.IE));
    //Caso a IE possua menos do que 12 dígitos, o AC deve preencher com espaços à direita. ER 2.21.08
    if Length(InscEst) < 12 then
      InscEst := PadRight(InscEst,12,' ');

    Gerador.wCampo(tcStrOrig, 'C12', 'IE', 2, 14, 1, InscEst, DSC_IE);

    Gerador.wCampo(tcStr, 'C13', 'IM', 01, 15, 0, CFeCanc.Emit.IM, DSC_IM);
  end;

  Gerador.wGrupo('/emit');
end;

procedure TCFeCancW.GerarEmitEnderEmit;
begin
  if not FApenasTagsAplicacao then
  begin
    Gerador.wGrupo('enderEmit', 'C05');
    Gerador.wCampo(tcStr, 'C06', 'xLgr   ', 02, 60, 1, CFeCanc.Emit.EnderEmit.xLgr, DSC_XLGR);
    Gerador.wCampo(tcStr, 'C07', 'nro    ', 01, 60, 1, ExecutarAjusteTagNro(FOpcoes.FAjustarTagNro, CFeCanc.Emit.enderEmit.nro), DSC_NRO);
    Gerador.wCampo(tcStr, 'C08', 'xCpl   ', 01, 60, 0, CFeCanc.Emit.enderEmit.xCpl, DSC_XCPL);
    Gerador.wCampo(tcStr, 'C09', 'xBairro', 02, 60, 1, CFeCanc.Emit.enderEmit.xBairro, DSC_XBAIRRO);
    Gerador.wCampo(tcStr, 'C10', 'xMun   ', 02, 60, 1, CFeCanc.Emit.enderEmit.xMun, DSC_XMUN);
    Gerador.wCampo(tcInt, 'C11', 'CEP    ', 08, 08, 1, CFeCanc.Emit.enderEmit.CEP, DSC_CEP);
    Gerador.wGrupo('/enderEmit');
  end;
end;

procedure TCFeCancW.GerarDest;
begin
  Gerador.wGrupo('dest', 'E01');

  if (CFeCanc.infCFe.versao < 0.07) or (not FApenasTagsAplicacao) then
    Gerador.wCampoCNPJCPF('E02', 'E03', CFeCanc.Dest.CNPJCPF, False);

  Gerador.wGrupo('/dest');
end;

procedure TCFeCancW.GerarTotal;
begin
  Gerador.wGrupo('total', 'W01');

  if not FApenasTagsAplicacao then
    Gerador.wCampo(tcDe2, 'W11', 'vCFe', 01, 15, 1, CFeCanc.Total.vCFe, DSC_VCFE);

  Gerador.wGrupo('/total');
end;

procedure TCFeCancW.GerarInfAdic;
begin
  if (CFeCanc.InfAdic.obsFisco.Count > 0) then
  begin
    Gerador.wGrupo('infAdic', 'Z01');
    (**)GerarInfAdicObsFisco;
    Gerador.wGrupo('/infAdic');
  end;
end;

procedure TCFeCancW.GerarInfAdicObsFisco;
var
  i: integer;
begin
  if not FApenasTagsAplicacao then
  begin
    if CFeCanc.InfAdic.obsFisco.Count > 10 then
      Gerador.wAlerta('Z03', 'obsFisco', DSC_OBSFISCO, ERR_MSG_MAIOR_MAXIMO + '10');

    for i := 0 to CFeCanc.InfAdic.obsFisco.Count - 1 do
    begin
      Gerador.wGrupo('obsFisco xCampo="' + trim(CFeCanc.InfAdic.obsFisco[i].xCampo) + '"', IIf(CFeCanc.infCFe.versao >= 0.08,'ZA02','Z04'));

      if length(trim(CFeCanc.InfAdic.obsFisco[i].xCampo)) > 20 then
        Gerador.wAlerta('Z04', 'xCampo', DSC_XCAMPO, ERR_MSG_MAIOR);

      if length(trim(CFeCanc.InfAdic.obsFisco[i].xCampo)) = 0 then
        Gerador.wAlerta('Z04', 'xCampo', DSC_XCAMPO, ERR_MSG_VAZIO);

      Gerador.wCampo(tcStr, 'Z05', 'xTexto', 01, 60, 1, CFeCanc.InfAdic.obsFisco[i].xTexto, DSC_XTEXTO);
      Gerador.wGrupo('/obsFisco');
    end;
  end;
end;

function TCFeCancW.GerarXml(ApenasTagsAplicacao: Boolean): boolean;
var
  Gerar: boolean;
  Grupo: String;
begin
  FApenasTagsAplicacao := ApenasTagsAplicacao;
  Gerador.LayoutArquivoTXT.Clear;

  {$IFDEF UNICODE}
   Gerador.ArquivoFormatoXML := '<' + ENCODING_UTF8 + '>';
   if Gerador.Opcoes.IdentarXML then
     Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML + #13#10;
  {$ELSE}
   Gerador.ArquivoFormatoXML := '';
  {$ENDIF}

  Gerador.ArquivoFormatoTXT := '';

  Gerador.wGrupo('CFeCanc');
  Grupo := 'infCFe';

  if not ApenasTagsAplicacao then
    Grupo := Grupo + ' versao="' + StringReplace(FormatFloat('0.00',CFeCanc.infCFe.versao),',','.',[rfReplaceAll])+'" ' +
                     ' Id="' + CFeCanc.infCFe.ID + '" ';

  Grupo := Grupo + ' chCanc="' + CFeCanc.infCFe.chCanc  + '"';

  Gerador.wGrupo( Grupo );

  if not ApenasTagsAplicacao then
  begin
    Gerador.wCampo(tcDatCFe, 'A07', 'dEmi', 10, 10, 0, CFeCanc.infCFe.dEmi, DSC_DEMI);
    Gerador.wCampo(tcHorCFe, 'A08', 'hEmi', 08, 08, 0, CFeCanc.infCFe.hEmi, DSC_HEMI);
  end;

  (**)GerarInfCFe;
  Gerador.wGrupo('/infCFe');
  //
  if FOpcoes.GerarTagAssinatura <> taNunca then
  begin
    Gerar := true;
    if FOpcoes.GerarTagAssinatura = taSomenteSeAssinada then
      Gerar := ((CFeCanc.signature.DigestValue <> '') and (CFeCanc.signature.SignatureValue <> '') and (CFeCanc.signature.X509Certificate <> ''));
    if FOpcoes.GerarTagAssinatura = taSomenteParaNaoAssinada then
      Gerar := ((CFeCanc.signature.DigestValue = '') and (CFeCanc.signature.SignatureValue = '') and (CFeCanc.signature.X509Certificate = ''));
    if Gerar then
    begin
      FCFeCanc.signature.URI := OnlyNumber(CFeCanc.infCFe.ID);
      FCFeCanc.signature.Gerador.Opcoes.IdentarXML := Gerador.Opcoes.IdentarXML;
      FCFeCanc.signature.GerarXML;
      Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML + FCFeCanc.signature.Gerador.ArquivoFormatoXML;
    end;
  end;
  Gerador.wGrupo('/CFeCanc');

  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

function TCFeCancW.ObterNomeArquivo: string;
begin
  Result := OnlyNumber(CFeCanc.infCFe.ID) + '-can-cfe.xml';
end;

end.
 
