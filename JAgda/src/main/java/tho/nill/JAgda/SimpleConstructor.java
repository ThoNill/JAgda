package tho.nill.JAgda;

public class SimpleConstructor extends AgdaConstructor {
	private ConstructorInstance instance;
	
	public SimpleConstructor(String simpleName, boolean visible,WithConstructorsType type,AgdaType returnType) {
		super(simpleName, visible, type,returnType);
		if (!type.isSubtype(returnType)) {
			throw new AgdaException(" Typ " + type + " passt ich zu Typ " + returnType);
		}
		instance = new ConstructorInstance(returnType, null,this);
	}

	@Override
	public AgdaInstance apply(AgdaInstance source) {
		return instance; 
	}

}
