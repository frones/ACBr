{******************************************************************************}
{ Projeto: Componente ACBrNFe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Nota Fiscal}
{ eletrônica - NFe - http://www.nfe.fazenda.gov.br                             }
{                                                                              }
{ Direitos Autorais Reservados (c) 2008 Wemerson Souto                         }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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

{*******************************************************************************
|* Historico
|*
|* 16/12/2008: Wemerson Souto
|*  - Doação do componente para o Projeto ACBr
*******************************************************************************}

{$I ACBr.inc}

unit ACBrNFeDANFEClass;

interface

uses
  SysUtils, Classes, ACBrBase,
  pcnNFe, pcnConversao;

type
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}	
  TCasasDecimais = class(TComponent)
  private
    fFormato : TDetFormato;
    FqCom: Integer;
    FvUnCom: Integer;
    FMask_qCom: String;
    FMask_vUnCom: String;

    procedure Set_qCom(AValue: Integer);
    procedure Set_vUnCom(AValue: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Formato : TDetFormato  read fFormato     write fFormato;
    property _qCom: Integer         read FQCom        write Set_qCom;
    property _vUnCom: Integer       read FvUnCom      write Set_vUnCom;
    property _Mask_qCom: String     read FMask_qCom   write FMask_qCom;
    property _Mask_vUnCom: String   read FMask_vUnCom write FMask_vUnCom;
  end;

  { TACBrNFeDANFEClass }
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}	
  TACBrNFeDANFEClass = class( TACBrComponent )
   private
    procedure SetNFE(const Value: TComponent);
    procedure ErroAbstract(NomeProcedure: String);
    procedure SetPathPDF(const Value: String);
    function GetPathPDF: String;
  protected
    FACBrNFe: TComponent;
    FLogo: String;
    FSistema: String;
    FUsuario: String;
    FPathPDF: String;
    FUsarSeparadorPathPDF: Boolean;
    FImpressora: String;
    FNomeDocumento: String;
    FImprimeNomeFantasia: Boolean;
    FImprimirTotalLiquido: Boolean;
    FMostrarPreview: Boolean;
    FMostrarStatus: Boolean;
    FTipoDANFE: TpcnTipoImpressao;
    FNumCopias: Integer;
    FExpandirLogoMarca: Boolean;
    FFax: String;
    FSite: String;
    FEmail: String;
    FImprimeDescPorc: Boolean;
    FProtocoloNFe: String;
    FLarguraBobina: Integer;
    FMargemInferior: Double;
    FMargemSuperior: Double;
    FMargemEsquerda: Double;
    FMargemDireita: Double;
    FCasasDecimais: TCasasDecimais;
    FExibeResumoCanhoto: Boolean;
    FExibeResumoCanhoto_Texto: String;
    FFormularioContinuo: Boolean;
    FTamanhoFonte_DemaisCampos: Integer;
    FProdutosPorPagina: Integer;
    FImprimirDetalhamentoEspecifico: Boolean;
    FNFeCancelada: Boolean;
    FImprimeItens: Boolean;    // Destinado exclusivamente ao DANFE da NFC-e
    FViaConsumidor : Boolean;  // Destinado exclusivamente ao DANFE da NFC-e
    FvTroco: Currency;
    FImprimeEmUmaLinha: Boolean;
    FUsaCodigoEanImpressao: Boolean;
    FImprimeDescAcrescItem: Boolean;
    FQRCodeLateral: Boolean;
    FImprimeLogoLateral: Boolean;

    FTamanhoLogoHeight: Integer;
    FTamanhoLogoWidth: Integer;
    FRecuoEndereco: Integer;
    FRecuoEmpresa: Integer;
    FLogoEmCima: Boolean;
    FTamanhoFonteEndereco: Integer;
    FRecuoLogo: Integer;

    FTributosSeparadamente : Boolean; //informação dos tributos separadamente
    FvTribFed: Currency; //total tributos federais
    FvTribEst: Currency; //total tributos estaduais
    FvTribMun: Currency; //total tributos municipais
    FFonteTributos: String;
    FChaveTributos: String;
    FImprimirTributos : Boolean;

    FPosCanhoto: TPosRecibo;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ImprimirDANFE(NFE: TNFe = nil); virtual;
    procedure ImprimirDANFECancelado(NFE: TNFe = nil); virtual;
    procedure ImprimirDANFEResumido(NFE: TNFe = nil); virtual;
    procedure ImprimirDANFEPDF(NFE: TNFe = nil); virtual;
    procedure ImprimirDANFEResumidoPDF(NFE: TNFe = nil); virtual;
    procedure ImprimirEVENTO(NFE: TNFe = nil); virtual;
    procedure ImprimirEVENTOPDF(NFE: TNFe = nil); virtual;
    procedure ImprimirINUTILIZACAO(NFE: TNFe = nil); virtual;
    procedure ImprimirINUTILIZACAOPDF(NFE: TNFe = nil); virtual;

    function ManterCodigo(scEAN, scProd: String): String;
    function ManterNomeImpresso(sXNome, sXFant: String): String;
    function FormatQuantidade(dValor: Double; dForcarDecimais: Boolean = True): String;
    function FormatValorUnitario(dValor: Double): String;
    function ManterUnidades(sUCom, sUTrib: String): String;
    function ManterQuantidades(dQCom, dQTrib: Double): String;
    function ManterValoresUnitarios(dVCom, dVTrib: Double): String;
    function ManterDocreferenciados( aNFE: TNFe;bImprimirDadosDocReferenciados: Boolean;
      sQuebraLinha: String = ' '): String;

  published
    property ACBrNFe: TComponent                     read FACBrNFe                        write SetNFE;
    property Logo: String                            read FLogo                           write FLogo;
    property Sistema: String                         read FSistema                        write FSistema;
    property Usuario: String                         read FUsuario                        write FUsuario;
    property PathPDF: String                         read GetPathPDF                      write SetPathPDF;
    property UsarSeparadorPathPDF: Boolean           read FUsarSeparadorPathPDF           write FUsarSeparadorPathPDF default False;
    property Impressora: String                      read FImpressora                     write FImpressora;
    property NomeDocumento: String                   read FNomeDocumento                  write FNomeDocumento;
    property MostrarPreview: Boolean                 read FMostrarPreview                 write FMostrarPreview;
    property MostrarStatus: Boolean                  read FMostrarStatus                  write FMostrarStatus;
    property TipoDANFE: TpcnTipoImpressao            read FTipoDANFE                      write FTipoDANFE;
    property NumCopias: Integer                      read FNumCopias                      write FNumCopias;
    property Fax: String                             read FFax                            write FFax;
    property Site: String                            read FSite                           write FSite;
    property Email: String                           read FEmail                          write FEmail;
    property ImprimeNomeFantasia: Boolean            read FImprimeNomeFantasia            write FImprimeNomeFantasia;
    property ImprimirDescPorc: Boolean               read FImprimeDescPorc                write FImprimeDescPorc;
    property ImprimirTotalLiquido: Boolean           read FImprimirTotalLiquido           write FImprimirTotalLiquido;
    property ProtocoloNFe: String                    read FProtocoloNFe                   write FProtocoloNFe;
    property LarguraBobina : Integer                 read FLarguraBobina                  write FLarguraBobina default 302;
    property MargemInferior: Double                  read FMargemInferior                 write FMargemInferior;
    property MargemSuperior: Double                  read FMargemSuperior                 write FMargemSuperior;
    property MargemEsquerda: Double                  read FMargemEsquerda                 write FMargemEsquerda;
    property MargemDireita: Double                   read FMargemDireita                  write FMargemDireita;
    property CasasDecimais: TCasasDecimais           read FCasasDecimais;
    property ExibirResumoCanhoto: Boolean            read FExibeResumoCanhoto             write FExibeResumoCanhoto;
    property ExibirResumoCanhoto_Texto: String       read FExibeResumoCanhoto_Texto       write FExibeResumoCanhoto_Texto;
    property FormularioContinuo: Boolean             read FFormularioContinuo             write FFormularioContinuo;
    property ExpandirLogoMarca: Boolean              read FExpandirLogoMarca              write FExpandirLogoMarca default false;
    property TamanhoFonte_DemaisCampos: Integer      read FTamanhoFonte_DemaisCampos      write FTamanhoFonte_DemaisCampos;
    property ProdutosPorPagina: Integer              read FProdutosPorPagina              write FProdutosPorPagina;
    property ImprimirDetalhamentoEspecifico: Boolean read FImprimirDetalhamentoEspecifico write FImprimirDetalhamentoEspecifico;
    property NFeCancelada: Boolean                   read FNFeCancelada                   write FNFeCancelada;
    property ImprimirItens: Boolean                  read FImprimeItens                   write FImprimeItens;
    property vTroco: Currency                        read FvTroco                         write FvTroco;
    property ViaConsumidor : Boolean                 read FViaConsumidor                  write FViaConsumidor;
    property TamanhoLogoHeight: Integer              read FTamanhoLogoHeight              write FTamanhoLogoHeight;
    property TamanhoLogoWidth: Integer               read FTamanhoLogoWidth               write FTamanhoLogoWidth;
    property RecuoEndereco: Integer                  read FRecuoEndereco                  write FRecuoEndereco;
    property RecuoEmpresa: Integer                   read FRecuoEmpresa                   write FRecuoEmpresa;
    property LogoemCima: Boolean                     read FLogoEmCima                     write FLogoEmCima;
    property TamanhoFonteEndereco: Integer           read FTamanhoFonteEndereco           write FTamanhoFonteEndereco;
    property RecuoLogo: Integer                      read FRecuoLogo                      write FRecuoLogo;
    property ImprimirTributos: Boolean               read FImprimirTributos               write FImprimirTributos default True;
    property TributosSeparadamente: Boolean          read FTributosSeparadamente          write FTributosSeparadamente default False;
    property vTribFed: Currency                      read FvTribFed                       write FvTribFed;
    property vTribEst: Currency                      read FvTribEst                       write FvTribEst;
    property vTribMun: Currency                      read FvTribMun                       write FvTribMun;
    property FonteTributos: String                   read FFonteTributos                  write FFonteTributos;
    property ChaveTributos: String                   read FChaveTributos                  write FChaveTributos;
    property PosCanhoto: TPosRecibo                  read FPosCanhoto                     write FPosCanhoto default prCabecalho;
    property ImprimeEmUmaLinha: Boolean              read FImprimeEmUmaLinha              write FImprimeEmUmaLinha default True;
    property ImprimeDescAcrescItem: Boolean          read FImprimeDescAcrescItem          write FImprimeDescAcrescItem default True;
    property UsaCodigoEanImpressao: Boolean          read FUsaCodigoEanImpressao          write FUsaCodigoEanImpressao default False;
    property QRCodeLateral: Boolean                  read FQRCodeLateral                  write FQRCodeLateral default False;
    property ImprimeLogoLateral: Boolean             read FImprimeLogoLateral             write FImprimeLogoLateral default False;

  end;

implementation

uses
  ACBrNFe, ACBrUtil,ACBrDFeUtil,ACBrValidador;

//Casas Decimais
constructor TCasasDecimais.Create(AOwner: TComponent);
begin
  inherited create( AOwner );
  FMask_qCom    := ',0.00';
  FMask_vUnCom  := ',0.00';
  FQCom         := 2;
  FvUnCom       := 2;
end;

destructor TCasasDecimais.Destroy;
begin

  inherited Destroy;
end;

procedure TCasasDecimais.Set_qCom(AValue: Integer);
begin
  if ((AValue >= 0) and
      (AValue <= 4))  then
    FqCom := AValue
  else
    FqCom := 2;
end;

procedure TCasasDecimais.Set_vUnCom(AValue: Integer);
begin
  if ((AValue >= 0) and
      (AValue <= 10))  then
    FvUnCom := AValue
  else
    FvUnCom := 2;
end;

//DANFE CLASS
constructor TACBrNFeDANFEClass.Create(AOwner: TComponent);
begin
  inherited create( AOwner );

  FACBrNFe    := nil;
  FLogo       := '';
  FSistema    := '';
  FUsuario    := '';
  FPathPDF    := '';
  FUsarSeparadorPathPDF := False;
  FImpressora := '';
  FNomeDocumento := '';
  FImprimirTotalLiquido := False;
  FMostrarPreview       := True;
  FMostrarStatus        := True;
  FNumCopias := 1;
  FFax       := '';
  FSite      := '';
  FEmail     := '';
  FImprimeDescPorc := False;
  FProtocoloNFe    := '';
  FLarguraBobina   := 302;
  FMargemInferior  := 0.8;
  FMargemSuperior  := 0.8;
  FMargemEsquerda  := 0.6;
  FMargemDireita   := 0.51;
  FExibeResumoCanhoto := false;
  FExibeResumoCanhoto_Texto := '';
  FFormularioContinuo := false;
  FTamanhoFonte_DemaisCampos := 8;
  FProdutosPorPagina := 0;
  FImprimeNomeFantasia            := False;
  FImprimirDetalhamentoEspecifico := true;
  FImprimirTotalLiquido:= True;
  FNFeCancelada := False;
  FCasasDecimais := TCasasDecimais.Create(self);
  FCasasDecimais.Name:= 'CasasDecimais';
  FPosCanhoto := prCabecalho;
  FImprimeItens := True;
  FViaConsumidor:= True;
  FvTroco       := 0.0;

  FImprimirTributos      := True;
  FTributosSeparadamente := False;
  FvTribFed:= 0.0;
  FvTribEst:= 0.0;
  FvTribMun:= 0.0;
  FChaveTributos := '';

  FImprimeEmUmaLinha     := True;
  FImprimeDescAcrescItem := True;
  FUsaCodigoEanImpressao := False;
  FQRCodeLateral         := False;
  FImprimeLogoLateral    := False;

  {$IFDEF COMPILER6_UP}
      FCasasDecimais.SetSubComponent( true );{ para gravar no DFM/XFM }
  {$ENDIF}
end;

destructor TACBrNFeDANFEClass.Destroy;
begin

  inherited Destroy;
end;

procedure TACBrNFeDANFEClass.ImprimirDANFE(NFE : TNFe = nil);
begin
  ErroAbstract('Imprimir');
end;

procedure TACBrNFeDANFEClass.ImprimirDANFECancelado(NFE: TNFe);
begin
  ErroAbstract('ImprimirCancelado');
end;

procedure TACBrNFeDANFEClass.ImprimirDANFEResumido(NFE : TNFe = nil);
begin
  ErroAbstract('ImprimirResumido');
end;

procedure TACBrNFeDANFEClass.ImprimirDANFEPDF(NFE : TNFe = nil);
begin
  ErroAbstract('ImprimirDANFEPDF');
end;

procedure TACBrNFeDANFEClass.ImprimirDANFEResumidoPDF(NFE: TNFe);
begin
  ErroAbstract('ImprimirDANFEResumidoPDF');
end;

procedure TACBrNFeDANFEClass.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (FACBrNFe <> nil) and (AComponent is TACBrNFe) then
     FACBrNFe := nil;
end;

procedure TACBrNFeDANFEClass.SetNFE(const Value: TComponent);
  Var OldValue : TACBrNFe;
begin
  if Value <> FACBrNFe then
  begin
     if Value <> nil then
        if not (Value is TACBrNFe) then
           raise EACBrNFeException.Create('ACBrDANFE.NFE deve ser do tipo TACBrNFe');

     if Assigned(FACBrNFe) then
        FACBrNFe.RemoveFreeNotification(Self);

     OldValue := TACBrNFe(FACBrNFe);   // Usa outra variavel para evitar Loop Infinito
     FACBrNFe := Value;                 // na remoção da associação dos componentes

     if Assigned(OldValue) then
        if Assigned(OldValue.DANFE) then
           OldValue.DANFE := nil;

     if Value <> nil then
     begin
        Value.FreeNotification(self);
        TACBrNFe(Value).DANFE := self;
     end;
  end;
end;

procedure TACBrNFeDANFEClass.SetPathPDF(const Value: String);
begin
  FPathPDF := PathWithDelim(Value);
end;

procedure TACBrNFeDANFEClass.ErroAbstract(NomeProcedure: String);
begin
  raise EACBrNFeException.Create( NomeProcedure + ' não implementado em: '+ClassName);
end;

function TACBrNFeDANFEClass.GetPathPDF: String;
var
   dhEmissao: TDateTime;
   DescricaoModelo: String;
   ANFe: TNFe;
begin
  if (csDesigning in ComponentState) then
  begin
    Result := FPathPDF;
    Exit;
  end;

  Result := Trim(FPathPDF);

  if EstaVazio(Result) then  // Se não pode definir o Parth, use o Path da Aplicaçao
    Result := PathWithDelim( ExtractFilePath(ParamStr(0))) + 'pdf';

  if FUsarSeparadorPathPDF then
  begin
    if Assigned(ACBrNFe) then  // Se tem o componente ACBrNFe
    begin
      if TACBrNFe(ACBrNFe).NotasFiscais.Count > 0 then  // Se tem alguma Nota carregada
      begin
        ANFe := TACBrNFe(ACBrNFe).NotasFiscais.Items[0].NFe;
        if TACBrNFe(ACBrNFe).Configuracoes.Arquivos.EmissaoPathNFe then
          dhEmissao := ANFe.Ide.dEmi
        else
          dhEmissao := Now;

        DescricaoModelo := '';
        if TACBrNFe(ACBrNFe).Configuracoes.Arquivos.AdicionarLiteral then
        begin
           case ANFe.Ide.modelo of
             0: DescricaoModelo := TACBrNFe(FACBrNFe).GetNomeModeloDFe;
             55: DescricaoModelo := 'NFe';
             65: DescricaoModelo := 'NFCe';
           end;
        end;

        Result := TACBrNFe(FACBrNFe).Configuracoes.Arquivos.GetPath(
                         Result,
                         DescricaoModelo,
                         ANFe.Emit.CNPJCPF,
                         dhEmissao,
                         DescricaoModelo);
      end;
    end;
  end;

  Result := PathWithDelim( Result );
end;

procedure TACBrNFeDANFEClass.ImprimirEVENTO(NFE: TNFe);
begin
  ErroAbstract('Imprimir');
end;

procedure TACBrNFeDANFEClass.ImprimirEVENTOPDF(NFE: TNFe);
begin
  ErroAbstract('ImprimirPDF');
end;

procedure TACBrNFeDANFEClass.ImprimirINUTILIZACAO(NFE: TNFe);
begin
  ErroAbstract('Imprimir');
end;

procedure TACBrNFeDANFEClass.ImprimirINUTILIZACAOPDF(NFE: TNFe);
begin
  ErroAbstract('ImprimirPDF');
end;

function TACBrNFeDANFEClass.FormatQuantidade(dValor: Double; dForcarDecimais: Boolean) : String;
begin
  if (Frac(dValor) > 0) or (dForcarDecimais) then
  begin
    case CasasDecimais.Formato of
      tdetInteger : Result := FormatFloatBr( dValor , FloatMask( CasasDecimais._qCom));
      tdetMascara : Result := FormatFloatBr( dValor , CasasDecimais._Mask_qCom);
    else
      Result := FormatFloatBr( dValor , FloatMask( CasasDecimais._qCom));
    end
  end
  else
    // caso contrário mostrar somente o número inteiro
    Result := FloatToStr( dValor );
end;


function TACBrNFeDANFEClass.FormatValorUnitario( dValor : Double ) : String;
begin
  // formatar conforme configurado
  case CasasDecimais.Formato of
    tdetInteger : Result := FormatFloatBr( dValor , FloatMask( CasasDecimais._vUnCom));
    tdetMascara : Result := FormatFloatBr( dValor , CasasDecimais._Mask_vUnCom);
    else
      Result := FormatFloatBr( dValor , FloatMask( CasasDecimais._vUnCom));
  end;
end;

function TACBrNFeDANFEClass.ManterCodigo( scEAN , scProd : String ) : String;
begin
  if (Length( scEAN ) > 0) and (UpperCase(Trim(scEAN)) <> 'SEM GTIN') and (UsaCodigoEanImpressao) then
    Result := Trim(scEAN)
  else
    Result := Trim(scProd);
end;

function TACBrNFeDANFEClass.ManterNomeImpresso( sXNome , sXFant : String ) : String;
begin
  if ( fImprimeNomeFantasia ) and ( sXFant <> '' ) then
    Result := sXFant
  else
    Result := sXNome;
end;

function TACBrNFeDANFEClass.ManterQuantidades(dQCom, dQTrib: Double): String;
begin
   Result := FormatQuantidade( dQCom );
   if dQTrib > 0 then
      Result := Result + #13#10 + FormatQuantidade( dQTrib );
end;

function TACBrNFeDANFEClass.ManterUnidades(sUCom, sUTrib: String): String;
begin
   Result := Trim( sUCom );
   if Trim( sUTrib ) <> '' then
      Result := Result + #13#10 + Trim( sUTrib );
end;

function TACBrNFeDANFEClass.ManterValoresUnitarios(dVCom,
  dVTrib: Double): String;
begin
   Result := FormatValorUnitario ( dVCom );
   if dVTrib > 0 then
      Result := Result + #13#10 + FormatValorUnitario ( dVTrib );
end;

function TACBrNFeDANFEClass.ManterDocreferenciados( aNFE: TNFe;bImprimirDadosDocReferenciados : Boolean; sQuebraLinha : String = ' ' ) : String;
// Informações de Documentos referenciados
  function DescrModeloNFe(chave: String):String;
  begin
    case StrToIntDef(Copy(chave, 21, 2),0) of
      59:   Result := 'CFe-SAT Ref.:';
      65:   Result := 'NFCe Ref.:';
      else  Result := 'NFe Ref.:';
    end;
  end;
  Function MontaLadoALado(  bExecuta : Boolean;
                            sResult : string;
                            sInicio : String;
                            sString : String ) : String;
  begin
    if bExecuta  then
    begin
      if sResult = '' then
        Result := sInicio
      else
      if pos(sInicio,sResult) = 0 then
        Result := sResult+', '+ sInicio
      else
        Result := sResult+', ';

      Result := Result + '(' + sString +')' ;
    end
    else
      Result := sResult;
  end;
var
  i : Integer;
begin
  Result  := '';
  if ( bImprimirDadosDocReferenciados ) and ( ANFe.Ide.NFref.Count > 0 ) then
  begin
    for i := 0 to (ANFe.ide.NFref.Count - 1) do
    begin
      Result := MontaLadoALado( ( ANFe.ide.NFref[i].refNFe <> '' ),
                                  Result,
                                  DescrModeloNFe(ANFe.ide.NFref[i].refNFe) ,
                                  FormatarChaveAcesso( ANFe.ide.NFref[i].refNFe ) );
      Result := MontaLadoALado( ( ANFe.ide.NFref[i].refCTe <> '' ),
                                  Result,
                                  'CTe Ref.:',
                                  FormatarChaveAcesso( ANFe.ide.NFref[i].refCTe ));
      Result := MontaLadoALado( ( ANFe.ide.NFref[i].RefECF.modelo <> ECFModRefVazio ) ,
                                  Result,
                                  'ECF Ref.:',
                                  ACBrStr('modelo: ' + ECFModRefToStr(ANFe.ide.NFref[i].RefECF.modelo) +
                                  ' ECF: ' +ANFe.ide.NFref[i].RefECF.nECF + ' COO: ' + ANFe.ide.NFref[i].RefECF.nCOO));
      Result := MontaLadoALado( ( ANFe.ide.NFref[i].RefNF.CNPJ <> '' ),
                                  Result,
                                  'NF Ref.:',
                                  ACBrStr('série: ' + IntTostr(ANFe.ide.NFref[i].RefNF.serie) +
                                  ' número: ' + IntTostr(ANFe.ide.NFref[i].RefNF.nNF) +
                                  ' emit: ' + FormatarCNPJouCPF(ANFe.ide.NFref[i].RefNF.CNPJ) +
                                  ' modelo: ' + IntTostr(ANFe.ide.NFref[i].RefNF.modelo)));
      Result := MontaLadoALado( ( ANFe.ide.NFref[i].RefNFP.nNF > 0 ),
                                  Result,
                                  'NFP Ref.:',
                                  ACBrStr('série: ' + IntTostr(ANFe.ide.NFref[i].RefNFP.serie) +
                                  ' número: ' + IntTostr(ANFe.ide.NFref[i].RefNFP.nNF) +
                                  ' modelo: ' + ANFe.ide.NFref[i].RefNFP.modelo +
                                  ' emit: ' + FormatarCNPJouCPF(ANFe.ide.NFref[i].RefNFP.CNPJCPF) +
                                  ' IE: ' + ANFe.ide.NFref[i].RefNFP.IE +
                                  ' UF: ' + CUFtoUF(ANFe.ide.NFref[i].RefNFP.cUF)));

    end;
    Result := Result + sQuebraLinha;
  end;
end;

end.
