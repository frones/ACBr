{*******************************************************************************}
{ Projeto: ACBrMonitor                                                          }
{  Executavel multiplataforma que faz uso do conjunto de componentes ACBr para  }
{ criar uma interface de comunicação com equipamentos de automacao comercial.   }
{                                                                               }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida                }
{                                                                               }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr     }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr       }
{                                                                               }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la  }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela   }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério)  }
{ qualquer versão posterior.                                                    }
{                                                                               }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM    }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU       }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor }
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)               }
{                                                                               }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto }
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,   }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.           }
{ Você também pode obter uma copia da licença em:                               }
{ http://www.opensource.org/licenses/gpl-license.php                            }
{                                                                               }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br }
{        Rua Cel.Aureliano de Camargo, 963 - Tatuí - SP - 18270-170             }
{                                                                               }
{*******************************************************************************}
unit CmdUnit;

{$mode objfpc}{$H+}

interface
Uses
  SysUtils, Classes, Math, ACBrMonitorConfig, ACBrLibResposta, ACBrMonitorConsts, DateUtils;

Const
   Objetos = '"ECF","CHQ","GAV","DIS","LCB","ACBR","BAL","ETQ","BOLETO","CEP","IBGE","EMAIL","SEDEX","NCM","NFE","CTE","MDFE","SAT","ESCPOS","GNRE","ESOCIAL","REINF","BPE","CNPJ","CPF","GTIN","NFSE"';
   TIME_EXPIRES = 60;
type

{ TACBrCmd }

TACBrCmd = class
private
  fsParams : TStringList ;
  fsComando: AnsiString;
  fsObjeto : AnsiString;
  fsMetodo : AnsiString;
  fsResposta: AnsiString;
  fsStart  : TDateTime;
  procedure SetComando(const Value: AnsiString);

public
  constructor Create;
  destructor Destroy; override ;

  function Params( Index : Integer) : AnsiString ;
  function Expired(): Boolean;

  property Comando : AnsiString read fsComando write SetComando ;
  property Objeto  : AnsiString read fsObjeto ;
  property Metodo  : AnsiString read fsMetodo ;
  property Resposta: AnsiString read fsResposta write fsResposta ;
  property Start   : TDateTime  read fsStart;
end;


{ TACBrObjeto }

TACBrObjeto = class
private
  fListaDeMetodos: TStringList;
  function getTipoResposta: TACBrLibRespostaTipo;
protected
  fpConfig: TMonitorConfig;
  fpCmd: TACBrCmd;

public
  constructor Create(AConfig: TMonitorConfig); virtual;
  destructor Destroy; override;

  procedure Executar(ACmd: TACBrCmd); virtual;

  property ListaDeMetodos: TStringList read fListaDeMetodos;
  property MonitorConfig: TMonitorConfig read fpConfig;
  property TpResp: TACBrLibRespostaTipo read getTipoResposta;
end;

TACBrEventoAntesImprimir = procedure(ShowPreview: Boolean) of object;
TACBrEventoDepoisImprimir = procedure of object;
TACBrEventoConfiguraDANFe = procedure(GerarPDF: Boolean; MostrarPreview : String) of  object;
TACBrEventoValidarIntegradorNFCe = procedure(ChaveNFe: String) of object;
TACBrEventoConfiguraDACTe = procedure(GerarPDF: Boolean; MostrarPreview : String) of  object;
TACBrEventoPrepararImpressaoSAT = procedure(NomeImpressora : String; GerarPDF : Boolean) of object;
TACBrEventoRespostaIntegrador = function():String of object;
TACBrEventoSubstituirVariaveis = function(const ATexto: String): String of object;
TACBrEventoConfiguraDABPe = procedure(GerarPDF: Boolean; MostrarPreview : String) of  object;
TACBrEventoConfiguraDANFSe = procedure(GerarPDF: Boolean; MostrarPreview : String) of  object;

{ TACBrObjetoDFe }

TACBrObjetoDFe = class(TACBrObjeto)
private
  FOnAntesDeImprimir: TACBrEventoAntesImprimir;
  FOnConfiguraDABPe: TACBrEventoConfiguraDABPe;
  FOnConfiguraDANFSe: TACBrEventoConfiguraDANFSe;
  FOnDepoisDeImprimir: TACBrEventoDepoisImprimir;
  FOnConfiguraDANFe: TACBrEventoConfiguraDANFe;
  FOnValidarIntegradorNFCe: TACBrEventoValidarIntegradorNFCe;
  FOnConfiguraDACTe: TACBrEventoConfiguraDACTe;
  FOnPrepararImpressaoSAT: TACBrEventoPrepararImpressaoSAT;
  FOnRespostaIntegrador: TACBrEventoRespostaIntegrador;
  FOnSubstituirVariaveis: TACBrEventoSubstituirVariaveis;
public
  procedure DoAntesDeImprimir(ShowPreview: Boolean);
  procedure DoDepoisDeImprimir;
  procedure DoConfiguraDANFe(GerarPDF: Boolean; MostrarPreview : String);
  procedure DoValidarIntegradorNFCe(ChaveNFe: String = '');
  procedure DoConfiguraDACTe(GerarPDF: Boolean; MostrarPreview : String);
  procedure DoPrepararImpressaoSAT(NomeImpressora : String; GerarPDF : Boolean = False);
  function  DoRespostaIntegrador():String;
  function  DoSubstituirVariaveis(const ATexto: String): String;
  procedure DoConfiguraDABPe(GerarPDF: Boolean; MostrarPreview : String);
  procedure DoConfiguraDANFSe(GerarPDF: Boolean; MostrarPreview : String);

  property OnAntesDeImprimir: TACBrEventoAntesImprimir read FOnAntesDeImprimir write FOnAntesDeImprimir;
  property OnDepoisDeImprimir: TACBrEventoDepoisImprimir read FOnDepoisDeImprimir write FOnDepoisDeImprimir;
  property OnConfiguraDANFe: TACBrEventoConfiguraDANFe read FOnConfiguraDANFe write FOnConfiguraDANFe;
  property OnValidarIntegradorNFCe: TACBrEventoValidarIntegradorNFCe read FOnValidarIntegradorNFCe write FOnValidarIntegradorNFCe;
  property OnConfiguraDACTe: TACBrEventoConfiguraDACTe read FOnConfiguraDACTe write FOnConfiguraDACTe;
  property OnPrepararImpressaoSAT: TACBrEventoPrepararImpressaoSAT read FOnPrepararImpressaoSAT write FOnPrepararImpressaoSAT;
  property OnRespostaIntegrador: TACBrEventoRespostaIntegrador read FOnRespostaIntegrador write FOnRespostaIntegrador;
  property OnSubstituirVariaveis: TACBrEventoSubstituirVariaveis read FOnSubstituirVariaveis write FOnSubstituirVariaveis;
  property OnConfiguraDABPe: TACBrEventoConfiguraDABPe read FOnConfiguraDABPe write FOnConfiguraDABPe;
  property OnConfiguraDANFSe: TACBrEventoConfiguraDANFSe read FOnConfiguraDANFSe write FOnConfiguraDANFSe;
end;

{ TACBrMetodo }

TACBrMetodo = class
protected
  fpCmd: TACBrCmd;
  fpObjetoDono: TACBrObjeto;
public
  constructor Create(ACmd: TACBrCmd; ObjetoDono: TACBrObjeto); virtual;
  procedure Executar; virtual;
end;

TACBrMetodoClass = class of TACBrMetodo;

implementation

uses StrUtils;

{ TACBrObjetoDFe }

procedure TACBrObjetoDFe.DoAntesDeImprimir(ShowPreview: Boolean);
begin
  if Assigned(FOnAntesDeImprimir) then
    FOnAntesDeImprimir(ShowPreview);
end;

procedure TACBrObjetoDFe.DoDepoisDeImprimir;
begin
  if Assigned(FOnDepoisDeImprimir) then
    FOnDepoisDeImprimir;
end;

procedure TACBrObjetoDFe.DoConfiguraDANFe(GerarPDF: Boolean; MostrarPreview : String);
begin
  if Assigned(FOnConfiguraDANFe) then
    FOnConfiguraDANFe(GerarPDF, MostrarPreview);
end;

procedure TACBrObjetoDFe.DoValidarIntegradorNFCe(ChaveNFe: String);
begin
  if Assigned(FOnValidarIntegradorNFCe) then
    FOnValidarIntegradorNFCe(ChaveNFe);
end;

procedure TACBrObjetoDFe.DoConfiguraDACTe(GerarPDF: Boolean; MostrarPreview : String);
begin
  if Assigned(FOnConfiguraDACTe) then
    FOnConfiguraDACTe(GerarPDF, MostrarPreview);
end;

procedure TACBrObjetoDFe.DoPrepararImpressaoSAT(NomeImpressora: String;
  GerarPDF: Boolean);
begin
  if Assigned(FOnPrepararImpressaoSAT) then
    FOnPrepararImpressaoSAT(NomeImpressora, GerarPDF);
end;

function TACBrObjetoDFe.DoRespostaIntegrador(): String;
begin
  if Assigned(FOnRespostaIntegrador) then
    Result:= FOnRespostaIntegrador();
end;

function TACBrObjetoDFe.DoSubstituirVariaveis(const ATexto: String): String;
begin
  if Assigned(FOnSubstituirVariaveis) then
    Result:= FOnSubstituirVariaveis(ATexto);
end;

procedure TACBrObjetoDFe.DoConfiguraDABPe(GerarPDF: Boolean;
  MostrarPreview: String);
begin
  if Assigned(FOnConfiguraDABPe) then
    FOnConfiguraDABPe(GerarPDF, MostrarPreview);
end;

procedure TACBrObjetoDFe.DoConfiguraDANFSe(GerarPDF: Boolean; MostrarPreview : String);
begin
  if Assigned(FOnConfiguraDANFSe) then
    FOnConfiguraDANFSe(GerarPDF, MostrarPreview);
end;

{ TACBrObjeto }

constructor TACBrObjeto.Create(AConfig: TMonitorConfig);
begin
  inherited Create;
  fpConfig := AConfig;
  fListaDeMetodos := TStringList.Create;
end;

destructor TACBrObjeto.Destroy;
begin
  fListaDeMetodos.Free;
  inherited Destroy;
end;

function TACBrObjeto.getTipoResposta: TACBrLibRespostaTipo;
begin
  Result := TACBrLibRespostaTipo(fpConfig.ACBrMonitor.TipoResposta);
end;


procedure TACBrObjeto.Executar(ACmd: TACBrCmd);
begin
  fpCmd := ACmd;

  if fListaDeMetodos.IndexOf(LowerCase(ACmd.Metodo)) < 0 then
    raise Exception.Create('Comando inválido (' + ACmd.Metodo + ')');
end;

{ TACBrMetodo }

constructor TACBrMetodo.Create(ACmd: TACBrCmd; ObjetoDono: TACBrObjeto);
begin
  inherited Create;
  fpCmd := ACmd;
  fpObjetoDono := ObjetoDono;
end;

procedure TACBrMetodo.Executar;
begin
  raise Exception.Create( ClassName+'.Execute, não implementado');
end;

{ TACBrCmd }

constructor TACBrCmd.Create;
begin
  fsParams := TStringList.Create ;
  fsStart := Now;
end;

destructor TACBrCmd.Destroy;
begin
  fsParams.Free ;
  inherited Destroy ;
end;

function TACBrCmd.Params(Index: Integer): AnsiString;
begin
  if Index > fsParams.Count-1 then
     Result := ''
  else
     Result := fsParams[Index] ;
end;

function TACBrCmd.Expired(): Boolean;
begin
  Result:= (MinutesBetween(Start, Now) > TIME_EXPIRES);
end;

procedure TACBrCmd.SetComando(const Value: AnsiString);
Var P,PaI,PaF,Pv : Integer ;
    wComando, wParam, wProxChar : AnsiString ;
begin
  fsMetodo   := '' ;
  fsObjeto   := '' ;
  fsResposta := '' ;
  fsParams.Clear ;

  fsComando := Value ;
  wComando  := Value ;

  { Validação p/ Versão Demonstacao }
  {$IFDEF Demo}
  if Expired() then
     raise Exception.Create(SErrTempoUsoExpirou) ;
  {$ENDIF}

  { Achando o Objeto }
  P := pos('.',wComando) ;
  if P = 0 then
     raise Exception.Create('Objeto nao definido') ;

  fsObjeto := UpperCase( Trim(copy(fsComando,1,P-1)) ) ;
  if pos('"'+fsObjeto+'"', Objetos) = 0 then
     raise Exception.Create('Objeto inválido: '+fsObjeto+sLineBreak+
                            ' Permitidos: '+Objetos ) ;

  wComando := copy(wComando, P+1, Length(wComando) ) ;

  { Achando o Método }
  P := pos('(',wComando) ;
  if P = 0 then
  begin
     { Verificando se é uma atribuição a propriedade Ex: "Ativo := true"
       Se for, transforma em "SetAtivo(True)"  }
     P := pos(':=',wComando) ;
     if P > 0 then
      begin
         wComando := 'Set'+Trim(copy(wComando,1,P-1))+'('+
                           Trim(copy(wComando,1,P+2))+')' ;
         P := pos('(',wComando) ;
      end
     else
        P := Length( wComando ) + 1 ;
  end ;

  fsMetodo := LowerCase( Trim(copy(wComando,1,P-1)) ) ;
  if fsMetodo = '' then
     raise Exception.Create('Metodo não informado') ;

  { Tem Parameteros ? }
  wComando := copy(wComando, P+1, Length(wComando) ) ;
  while Length(wComando) > 0 do
  begin
     PaI := pos('"',wComando) ;
     Pv  := pos(',',wComando) ;  { Procurando o Fim do parametro }
     if Pv = 0 then
        Pv := pos(')',wComando) ;
     if Pv = 0 then
        Pv := Length( wComando ) + 1;

     if (PaI <> 0) and (PaI < Pv) then  { Tem aspas no Inicio do Comando }
      begin                             { Entao procure a proxima Aspas }
       { Verificando se a aspas é seguida de ',' ou ')' ou fim do comando
          Se não for, então a aspas não é um delimitador de String e sim
          faz parte do texto da String }
        PaF := PaI ;
        wProxChar := ' ' ;
        while (PaF <> 0) and
              (wProxChar <> '') and
              (pos(wProxChar, ',)') = 0) do
        begin
           if wProxChar = '"' then
              Inc( PaF ) ;
           PaF := PaF + max(Pos('"',copy(wComando, PaF+1, Length(Wcomando)) ),1) ;
           wProxChar := copy(Trim(copy(wComando, PaF+1 , Length(Wcomando))),1,1) ;
        end ;

        if PaF = 0 then
           raise Exception.Create('Parametro Inválido. String não terminada');

        wParam := copy(wComando, PaI+1 , PaF-PaI-1 ) ;
        Pv := PosEx(',', wComando, PaF+1 ) ;
        if Pv = 0 then
           Pv := Length( wComando ) + 1;
      end
     else
        wParam := copy(wComando, 1, Pv-1 ) ;

     { convertendo as aspas duplas "", para simples " }
     wParam := StringReplace(wParam,'""','"',[rfReplaceAll]) ;

     fsParams.Add( wParam ) ;
     wComando := copy(wComando, Pv+1, Length( wComando ) ) ;
  end ;
end;

end.




