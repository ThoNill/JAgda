package tho.nill.JAgda;

/*
 * data Nat : Set where
 *     zero : Nat
 *     suc : Nat -> Nat  
 * 
 */
public class FunctionalConstructor extends AgdaConstructor {
	private FunctionType functionType;
	
	public FunctionalConstructor(String simpleName, boolean visible,WithConstructorsType type,FunctionType functionType) {
		super(simpleName, visible, type,functionType.getTarget());
		this.functionType = functionType;
	}

	@Override
	public AgdaInstance apply(AgdaInstance source) {
		if (!functionType.getSource().isInstance(source)) {
			throw new AgdaException(" die Instanz " + source + " gehört nicht zur Sourcetyp " + functionType);
		}
		return new ConstructorInstance(getReturnType(), source,this);
	}

}
