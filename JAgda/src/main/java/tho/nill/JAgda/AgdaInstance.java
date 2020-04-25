package tho.nill.JAgda;

public class AgdaInstance {

	private AgdaType type;
	private Object instance;

	public AgdaInstance(AgdaType type, Object instance) {
		super();
		this.type = type;
		this.instance = instance;
		check();
	}

	private void check() {
		if (!type.isInstance(this)) {
			throw new AgdaException(" Das Object " + instance + " ist nicht vom Typ " + type);
		}
	}

	public AgdaType getType() {
		return type;
	}

	public Object getInstance() {
		return instance;
	}

	public boolean leq(AgdaInstance b) {
		return leq(asArray(this),asArray(b));
	}
	
	public boolean leq(AgdaInstance ar[],AgdaInstance br[]) {
		int ia = ar.length-1;
		int ib = br.length-1;
		boolean gleich = true;
		while(ia>=0 && ib >=0 && gleich) {
			AgdaInstance a = ar[ia];
			AgdaInstance b = br[ib];
			gleich = gleich && a.equals(b);
			ia--;
			ib--;
		}
		return ia ==-1 && gleich;
	}

	public AgdaInstance[] asArray(AgdaInstance a) {
		AgdaInstance ar[] = new AgdaInstance[depth(a)+1];
		int index = 0;

		while (a != null) {
			ar[index] = a;
			Object o = a.getInstance();
			if (o != null && o instanceof AgdaInstance) {
				a = ((AgdaInstance) o);
			} else {
				return ar;
			}
			index++;
		}
		return ar;
	}

	public int depth(AgdaInstance a) {
		int depth = 0;
		while (a != null) {
			Object o = a.getInstance();
			if (o != null && o instanceof AgdaInstance) {
				a = ((AgdaInstance) o);
			} else {
				return depth;
			}
			depth++;
		}
		return depth;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((instance == null) ? 0 : instance.hashCode());
		result = prime * result + ((type == null) ? 0 : type.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		AgdaInstance other = (AgdaInstance) obj;
		if (instance == null) {
			if (other.instance != null)
				return false;
		} else if (!instance.equals(other.instance))
			return false;
		if (type == null) {
			if (other.type != null)
				return false;
		} else if (!type.equals(other.type))
			return false;
		return true;
	}

}
