package info.kgeorgiy.ja.leontev.arrayset;

import java.util.*;

public class ArraySet<E> extends AbstractSet<E> implements SortedSet<E> {
    private final ArrayList<E> arrayList;
    private final Comparator<? super E> cmp;

    public ArraySet() {
        this(Collections.emptyList(), null);
    }

    public ArraySet(Collection<? extends E> collection) {
        this(collection, null);
    }

    public ArraySet(Collection<? extends E> collection, Comparator<? super E> comparator) {
        SortedSet<E> treeSet = new TreeSet<>(comparator);
        treeSet.addAll(collection);
        this.arrayList = new ArrayList<>(treeSet);
        this.cmp = comparator;
    }

    @Override
    public Iterator<E> iterator() {
        return arrayList.iterator();
    }

    @Override
    public Comparator<? super E> comparator() {
        return cmp;
    }

    @Override
    public int size() {
        return arrayList.size();
    }

    @Override
    public boolean isEmpty() {
        return arrayList.isEmpty();
    }

    @Override
    @SuppressWarnings("unchecked")
    public boolean contains(Object o) {
        int res = Collections.binarySearch(arrayList, (E) o, cmp);
        return res >= 0;
    }

    @Override
    public E first() {
        if (isEmpty()) {
            throw new NoSuchElementException();
        }
        return arrayList.get(0);
    }

    @Override
    public E last() {
        if (isEmpty()) {
            throw new NoSuchElementException();
        }
        return arrayList.get(arrayList.size() - 1);
    }

    @Override
    public Object[] toArray() {
        return super.toArray();
    }

    @Override
    public <T> T[] toArray(T[] a) {
        return super.toArray(a);
    }

    @Override
    public boolean add(E e) {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean addAll(Collection<? extends E> c) {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean remove(Object o) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void clear() {
        throw new UnsupportedOperationException();
    }

    private int getInclusiveIndex(E e) {
        int res = Collections.binarySearch(arrayList, e, cmp);
        if (res >= 0) {
            return res;
        }
        return -1 * res - 1;
    }

    @Override
    public SortedSet<E> subSet(E fromElement, E toElement) {
        if (cmp != null && cmp.compare(fromElement, toElement) > 0) {
            throw new IllegalArgumentException();
        }
        return new ArraySet<>(arrayList.subList(
                getInclusiveIndex(fromElement),
                getInclusiveIndex(toElement))
        );
    }

    @Override
    public SortedSet<E> headSet(E toElement) {
        return new ArraySet<>(arrayList.subList(
                0,
                getInclusiveIndex(toElement))
        );
    }

    public SortedSet<E> tailSet(E fromElement) {
        return new ArraySet<>(arrayList.subList(
                getInclusiveIndex(fromElement),
                size())
        );
    }

    @Override
    public String toString() {
        return super.toString();
    }
}
